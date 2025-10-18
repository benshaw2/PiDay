// script.js — WebR version (matching current index.html layout)
import { WebR } from "https://webr.r-wasm.org/latest/webr.mjs";

const webR = new WebR();

// DOM references
const nameInput = document.getElementById("nameInput");
const diamInput = document.getElementById("diamInput");
const circInput = document.getElementById("circInput");
const addRowBtn = document.getElementById("addRowBtn");
const clearBtn = document.getElementById("clearBtn");
const fitBtn = document.getElementById("fitBtn");
const downloadCsvBtn = document.getElementById("downloadCsvBtn");
const downloadPlotBtn = document.getElementById("downloadPlotBtn");
const modelSelect = document.getElementById("modelSelect");
const formatSelect = document.getElementById("formatSelect");
const dataTable = document.getElementById("dataTable").querySelector("tbody");
const piBox = document.getElementById("piBox");
const plotImg = document.getElementById("plotImg");
const msg = document.getElementById("msg");

let rows = [];

// --- Helper: unique ID
const uid = () => Math.random().toString(36).slice(2, 9);

// --- Render table ---
function renderTable() {
  dataTable.innerHTML = "";
  if (rows.length === 0) {
    const emptyRow = document.createElement("tr");
    emptyRow.innerHTML = `<td colspan="4" style="color:#999;text-align:center;padding:0.5rem;">No data yet — add a measurement above.</td>`;
    dataTable.appendChild(emptyRow);
    return;
  }

  rows.forEach((r) => {
    const tr = document.createElement("tr");
    tr.innerHTML = `
      <td>${r.name || ""}</td>
      <td>${r.diameter || ""}</td>
      <td>${r.circumference || ""}</td>
      <td><button class="removeBtn">✖</button></td>
    `;
    tr.querySelector(".removeBtn").onclick = () => {
      rows = rows.filter((x) => x.id !== r.id);
      renderTable();
    };
    dataTable.appendChild(tr);
  });
}

// --- Add and clear data ---
addRowBtn.onclick = () => {
  const name = nameInput.value.trim();
  const diam = parseFloat(diamInput.value);
  const circ = parseFloat(circInput.value);

  if (!name || isNaN(diam) || isNaN(circ)) {
    alert("Please fill all fields with valid values.");
    return;
  }

  rows.push({ id: uid(), name, diameter: diam, circumference: circ });
  nameInput.value = "";
  diamInput.value = "";
  circInput.value = "";
  renderTable();
};

clearBtn.onclick = () => {
  rows = [];
  renderTable();
  piBox.textContent = "—";
  plotImg.src = "";
  msg.textContent = "";
};

// --- Download CSV (simplified quote-safe) ---
downloadCsvBtn.onclick = () => {
  if (rows.length === 0) {
    alert("No data to download.");
    return;
  }

  const header = "Name,Diameter,Circumference\n";
  const body = rows.map(r => {
    const name = (r.name || "").replace(/"/g, '""'); // escape quotes
    return `"${name}",${r.diameter},${r.circumference}`;
  }).join("\n");

  const blob = new Blob([header + body], { type: "text/csv;charset=utf-8;" });
  const a = document.createElement("a");
  a.href = URL.createObjectURL(blob);
  a.download = "pi_data.csv";
  a.click();
  URL.revokeObjectURL(a.href);
};

// --- Initialize WebR ---
async function initWebR() {
  await webR.init();
  try {
    const code = await (await fetch("pi_core.R")).text();
    await webR.evalR(code);
    msg.textContent = "R core loaded (online mode). Random effects disabled.";
  } catch (err) {
    msg.textContent = "Error loading R core file.";
    console.error(err);
  }
}

fitBtn.onclick = async () => {
  if (rows.length < 2) {
    alert("Need at least two data points.");
    return;
  }

  piBox.textContent = "Computing…";
  msg.textContent = "";

  const modelType = parseInt(modelSelect.value);
  const fmt = formatSelect.value || "png";

  // Build CSV for WebR
  const lines = ["Name,Diameter,Circumference"];
  for (const r of rows) {
    const name = String(r.name || "").replace(/"/g, '""');
    const d = Number(r.diameter);
    const c = Number(r.circumference);
    lines.push(`"${name}",${isNaN(d) ? "" : d},${isNaN(c) ? "" : c}`);
  }
  const csvText = lines.join("\n");

  // Write data.csv into WebR FS
  const encoder = new TextEncoder();
  try {
    webR.FS.writeFile("data.csv", encoder.encode(csvText));
  } catch (e) {
    try { webR.FS.writeFileSync("data.csv", encoder.encode(csvText)); } 
    catch (e2) {
      console.error("Failed to write data.csv:", e2);
      alert("Internal error: cannot write data file to WebR filesystem.");
      return;
    }
  }

  // --- Fit model in R and get string output ---
  let res;
  try {
    const outR = await webR.evalR(`
      df <- try(read.csv("data.csv", stringsAsFactors = FALSE), silent = TRUE)
      if (!inherits(df, "try-error")) {
        df$Diameter <- as.numeric(df$Diameter)
        df$Circumference <- as.numeric(df$Circumference)
      } else {
        df <- NULL
      }
      outStr <- fit_pi_model(df, ${modelType})
      as.character(outStr)
    `);

    //const outVec = await outR.toJs();
    //const outStr = outVec[0];
    //const [okStr, message, slopeStr, interceptStr] = outStr.split("|");
    
    //console.log(await outR.toJs());
    //const outStr = String(await outR.toJs());  // string directly
    //const [okStr, message, slopeStr, interceptStr] = outStr.split("|");
    
    
    const outVec = await outR.toJs();
    //console.log("Raw R output:", outVec);

    //const outStr = Array.isArray(outVec) ? outVec[0] : String(outVec);
    const outStr = outVec.values?.[0] ?? "";
    const [okStr, message, slopeStr, interceptStr] = outStr.split("|");


    res = {
      ok: okStr === "TRUE",
      message,
      slope: parseFloat(slopeStr),
      intercept: parseFloat(interceptStr)
    };

    //console.log("Parsed model result:", res);

  } catch (e) {
    console.error("Failed to compute model:", e);
    piBox.textContent = "Model failed";
    msg.textContent = "Cannot compute model.";
    return;
  }

  if (!res.ok || !isFinite(res.slope)) {
    piBox.textContent = "Model failed";
    msg.textContent = res.message || "Error fitting model.";
    return;
  }

  piBox.textContent = `Estimated π ≈ ${res.slope.toFixed(4)}`;
  msg.textContent = res.message || "";

// --- Generate plot ---
const file = fmt === "svg" ? "plot.svg" : "plot.png";

if (fmt === "svg") {
  const svgR = await webR.evalR(`
    {
      make_pi_plot(df, slope = ${res.slope}, intercept = ${res.intercept},
                   outfile = "${file}", width = 700, height = 500)
      readChar("${file}", nchars = file.info("${file}")$size)
    }
  `);

  const svgTxt = await svgR.toJs();
  plotImg.src = `data:image/svg+xml;base64,${btoa(unescape(encodeURIComponent(svgTxt.values[0])))}`;
} else {
  try {
    const result = await webR.captureR(`
      make_pi_plot(df, slope = ${res.slope}, intercept = ${res.intercept},
                   outfile = "${file}", width = 700, height = 500)
      if (file.exists("${file}")) {
        img_data <- readBin("${file}", "raw", file.info("${file}")$size)
      } else {
        img_data <- raw(0)
      }
      img_data
    `);

    const bytes = await result.result.toJs();
    //console.log("Raw PNG bytes:", bytes);

    if (!bytes || bytes.length === 0) {
      console.error("PNG file exists but returned empty or invalid raw vector.");
      plotImg.alt = "Plot failed to render.";
    } else {
      const binaryString = String.fromCharCode(...bytes);
      const base64 = btoa(binaryString);
      plotImg.src = `data:image/png;base64,${base64}`;
    }
  } catch (e) {
    console.error("Error rendering PNG plot:", e);
    plotImg.alt = "Plot failed to render.";
  }
}
};





// --- Download plot ---
downloadPlotBtn.onclick = () => {
  if (!plotImg.src) {
    alert("No plot to download.");
    return;
  }
  const a = document.createElement("a");
  const ext = formatSelect.value || "png";
  a.href = plotImg.src;
  a.download = `pi_plot.${ext}`;
  a.click();
};

// --- Boot ---
(function boot() {
  renderTable();
  initWebR();
})();

