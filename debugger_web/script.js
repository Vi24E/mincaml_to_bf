const STATE = {
    isRunning: false,
    ops: [],
    runInterval: null
};

// Elements
const elCodeList = document.getElementById('code-list');
const elStatus = document.getElementById('status-text');
const elPcIndicator = document.getElementById('pc-indicator');

// Init
async function init() {
    await fetchState(true);
    setupEvents();
}

function setupEvents() {
    document.getElementById('btn-step').addEventListener('click', () => {
        step(1);
    });

    document.getElementById('btn-next-large').addEventListener('click', async () => {
        await fetch('/step_large', { method: 'POST' });
        fetchState();
    });
    
    document.getElementById('btn-next-small').addEventListener('click', async () => {
        await fetch('/step_small', { method: 'POST' });
        fetchState();
    });

    document.getElementById('btn-run').addEventListener('click', () => {toggleRun();});
    document.getElementById('btn-reset').addEventListener('click', reset);
}

async function fetchState(isInit = false) {
    let url = '/state';
    if (isInit) url += '?init=1';
    
    try {
        const res = await fetch(url);
        const data = await res.json();
        
        if (data.ops) {
            STATE.ops = data.ops;
            renderCode(data.ops);
        }
        
        updateUI(data);
        
        // Metadata text
        if (isInit) {
            document.getElementById('reg-start-addr').textContent = data.reg_start !== null ? data.reg_start : 'None';
            document.getElementById('buf-start-addr').textContent = data.buffer_start !== null ? data.buffer_start : 'None';
            document.getElementById('var-start-addr').textContent = data.var_start !== null ? data.var_start : 'None';
        }
        
        return data;
    } catch (e) {
        console.error("Fetch error", e);
        elStatus.textContent = "Error fetching state";
    }
}

async function step(count = 1) {
    try {
        const res = await fetch('/step', {
            method: 'POST',
            body: JSON.stringify({ count }),
            headers: { 'Content-Type': 'application/json' }
        });
        const data = await res.json();
        
        // Poll status immediately after step
        await fetchState();
        
        return data.steps_executed;
    } catch (e) {
        console.error("Step error", e);
    }
}

async function toggleRun() {
    STATE.isRunning = !STATE.isRunning;
    const btn = document.getElementById('btn-run');
    
    if (STATE.isRunning) {
        btn.textContent = "Pause";
        btn.classList.add('active');
        runLoop();
    } else {
        btn.textContent = "Run";
        btn.classList.remove('active');
    }
}

async function runLoop() {
    if (!STATE.isRunning) return;
    
    // Execute a chunk of steps
    const steps = await step(100); // 100 steps per frame
    
    if (steps > 0 && STATE.isRunning) {
        requestAnimationFrame(runLoop);
    } else {
        // Stop if finished or error
        if (STATE.isRunning) toggleRun();
    }
}

async function reset() {
    if (STATE.isRunning) toggleRun();
    await fetch('/reset', { method: 'POST' });
    await fetchState(true);
}

// Rendering
function renderCode(ops) {
    elCodeList.innerHTML = '';
    // Use fragment for perf
    const frag = document.createDocumentFragment();
    
    ops.forEach((op, idx) => {
        const div = document.createElement('div');
        div.className = 'code-item';
        div.id = `op-${idx}`;
        div.textContent = op; // Just the op char
        div.title = `${idx}: ${op}`; // Tooltip shows index
        frag.appendChild(div);
    });
    
    elCodeList.appendChild(frag);
}

function updateUI(data) {
    // Status
    elStatus.textContent = `Step: ${data.step_count} | PC: ${data.pc} | Ptr: ${data.ptr}`;
    elPcIndicator.textContent = `(${data.pc})`;

    // Highlight PC
    const prevActive = elCodeList.querySelector('.active');
    if (prevActive) prevActive.classList.remove('active');
    
    const currActive = document.getElementById(`op-${data.pc}`);
    if (currActive) {
        currActive.classList.add('active');
        // Auto-scroll disabled by user request
    }
    
    // Render Memory
    renderMemGrid(document.getElementById('mem-tape'), data.memory.local_tape, data.ptr);
    renderMemGrid(document.getElementById('mem-registers'), data.memory.registers, data.ptr);
    renderMemGrid(document.getElementById('mem-buffer'), data.memory.buffer, data.ptr);
    
    // Context Display
    if (data.context) {
        document.getElementById('ctx-large').textContent = data.context.large;
        document.getElementById('ctx-small').textContent = data.context.small;
    }
    
    // Variables
    if (data.variables) {
        renderVars(document.getElementById('mem-vars'), data.variables);
    }
}

function renderVars(container, vars) {
    container.innerHTML = '';
    const frag = document.createDocumentFragment();
    
    vars.forEach(v => {
        const div = document.createElement('div');
        div.className = 'var-item';
        div.innerHTML = `<span class="var-id">Var ${v.id}</span>: <span class="var-val">${v.value}</span> <span class="var-addr">(@${v.addr})</span>`;
        frag.appendChild(div);
    });
    container.appendChild(frag);
}

function renderMemGrid(container, chunk, ptr) {
    if (!chunk) {
        container.innerHTML = '<div style="padding:10px; color:#666;">Not Available</div>';
        return;
    }
    
    container.innerHTML = '';
    
    chunk.data.forEach((val, i) => {
        const addr = chunk.start + i;
        const div = document.createElement('div');
        div.className = 'mem-cell';
        if (val !== 0) div.classList.add('nonzero');
        if (addr === ptr) div.classList.add('ptr');
        
        div.innerHTML = `
            <span class="mem-idx">${addr}</span>
            <span class="mem-val">${val}</span>
        `;
        container.appendChild(div);
    });
}

// Start
init();
