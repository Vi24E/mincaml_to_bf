const STATE = {
    isRunning: false,
    isFastRunning: false,
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

    document.getElementById('btn-run').addEventListener('click', () => {toggleRun(false);});
    document.getElementById('btn-run-fast').addEventListener('click', () => {toggleRun(true);});
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
            document.getElementById('stack-start-addr').textContent = data.stack_start !== null ? data.stack_start : 'None';
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

async function toggleRun(fast = false) {
    if (STATE.isRunning) {
        // Stop
        STATE.isRunning = false;
        STATE.isFastRunning = false;
        
        const btn = document.getElementById('btn-run');
        const btnFast = document.getElementById('btn-run-fast');
        
        btn.textContent = "Run";
        btn.classList.remove('active');
        btnFast.textContent = "Fast Run";
        btnFast.classList.remove('active');
    } else {
        // Start
        STATE.isRunning = true;
        STATE.isFastRunning = fast;
        
        const btn = fast ? document.getElementById('btn-run-fast') : document.getElementById('btn-run');
        btn.textContent = "Pause";
        btn.classList.add('active');
        
        runLoop();
    }
}

async function runLoop() {
    if (!STATE.isRunning) return;
    
    let steps = 0;
    if (STATE.isFastRunning) {
        // Fast mode: use /step_huge (100k steps)
        try {
            const res = await fetch('/step_huge', { method: 'POST' });
            const data = await res.json();
            steps = data.steps_executed;
            await fetchState();
        } catch (e) {
            console.error("Fast step error", e);
        }
    } else {
        // Normal mode: 100 steps
        steps = await step(100); 
    }
    
    if (steps > 0 && STATE.isRunning) {
        requestAnimationFrame(runLoop);
    } else {
        // Stop if finished or error
        if (STATE.isRunning) toggleRun(false);
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
    renderActivatedCells(document.getElementById('mem-activated'), data.memory.activated_cells, data.ptr);
    renderMemGrid(document.getElementById('mem-tape'), data.memory.local_tape, data.ptr);
    renderMemGrid(document.getElementById('mem-registers'), data.memory.registers, data.ptr);
    renderMemGrid(document.getElementById('mem-buffer'), data.memory.buffer, data.ptr);
    // renderMemGrid(document.getElementById('mem-stack'), data.memory.stack, data.ptr);
    
    // Context Display
    if (data.context) {
        document.getElementById('ctx-large').textContent = data.context.large;
        document.getElementById('ctx-small').textContent = data.context.small;
    }
    
    // Variables
    if (data.variables) {
        renderVars(document.getElementById('mem-vars'), data.variables);
    }

    // Stack Values
    if (data.stack_values) {
        // Reuse renderVars style but maybe specific container or reuse renderVars if format is same
        // Stack values are list of {id, addr, value}, same as vars basically.
        // Let's reuse renderVars but slightly modified or just use it.
        // renderVars uses class 'var-item'. maybe we want 'stack-item'?
        // The user just wants to see them.
        renderVars(document.getElementById('mem-stack'), data.stack_values, "Stack");
    }
}

function renderVars(container, vars, prefix = "Var") {
    container.innerHTML = '';
    const frag = document.createDocumentFragment();
    
    vars.forEach(v => {
        const div = document.createElement('div');
        div.className = 'var-item';
        div.innerHTML = `<span class="var-id">${prefix} ${v.id}</span>: <span class="var-val">${v.value}</span> <span class="var-addr">(@${v.addr})</span>`;
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

function renderActivatedCells(container, chunk, ptr) {
    if (!chunk) {
        container.innerHTML = '<div style="padding:10px; color:#666;">Not Available</div>';
        return;
    }

    container.innerHTML = '';
    
    // Create two rows
    const rowMain = document.createElement('div');
    rowMain.className = 'mem-row';
    const rowAux = document.createElement('div');
    rowAux.className = 'mem-row';
    
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
        
        if (i % 2 === 0) {
            rowMain.appendChild(div);
        } else {
            rowAux.appendChild(div);
        }
    });
    
    container.appendChild(rowMain);
    container.appendChild(rowAux);
}

// Start
init();
