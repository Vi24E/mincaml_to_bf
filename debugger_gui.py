#!/usr/bin/env python3
import sys
import tkinter as tk
from tkinter import ttk, font
import threading
import queue
from debugger import Debugger

class DebuggerGUI:
    def __init__(self, root, filename):
        self.root = root
        self.filename = filename
        self.root.title(f"Brainfuck Debugger - {filename}")
        self.root.geometry("1000x800")
        self.root.minsize(800, 600)
        
        # State
        self.dbg = None
        self.is_running = False
        self.load_queue = queue.Queue()
        
        # Fonts
        self.setup_fonts()
        
        # Initial Loading UI
        self.loading_frame = ttk.Frame(self.root)
        self.loading_frame.pack(fill=tk.BOTH, expand=True)
        
        self.status_label = ttk.Label(self.loading_frame, text="Starting debugger...", font=self.header_font)
        self.status_label.pack(pady=20)
        
        self.progress = ttk.Progressbar(self.loading_frame, mode='indeterminate')
        self.progress.pack(pady=10, padx=50, fill=tk.X)
        self.progress.start()
        
        # Start Loading Thread
        threading.Thread(target=self.load_data_thread, daemon=True).start()
        
        # Start polling for completion
        self.root.after(100, self.check_load_queue)

    def setup_fonts(self):
        # Fallback fonts just in case
        try:
            self.code_font = font.Font(family="Menlo", size=12)
            self.mem_font = font.Font(family="Courier", size=11)
            self.header_font = font.Font(family="Helvetica", size=10, weight="bold")
        except:
             self.code_font = font.Font(family="Courier", size=10)
             self.mem_font = font.Font(family="Courier", size=10)
             self.header_font = font.Font(size=10, weight="bold")

    def load_data_thread(self):
        try:
            with open(self.filename, 'r') as f:
                code_str = f.read()
            
            # This is the heavy part
            dbg = Debugger(code_str)
            self.load_queue.put(("SUCCESS", dbg))
        except Exception as e:
            self.load_queue.put(("ERROR", str(e)))

    def check_load_queue(self):
        try:
            msg, data = self.load_queue.get_nowait()
            if msg == "SUCCESS":
                self.dbg = data
                self.loading_frame.destroy()
                self.setup_main_ui()
                self.update_view()
            elif msg == "ERROR":
                self.status_label.config(text=f"Error: {data}", foreground="red")
                self.progress.stop()
        except queue.Empty:
            # Keep polling
            self.root.after(100, self.check_load_queue)

    def setup_main_ui(self):
        # --- Toolbar ---
        toolbar = ttk.Frame(self.root, padding=5)
        toolbar.pack(side=tk.TOP, fill=tk.X)
        
        ttk.Button(toolbar, text="Step", command=self.step).pack(side=tk.LEFT, padx=2)
        ttk.Button(toolbar, text="Run", command=self.toggle_run).pack(side=tk.LEFT, padx=2)
        ttk.Button(toolbar, text="Reset", command=self.reset).pack(side=tk.LEFT, padx=2)
        
        self.status_var = tk.StringVar(value=f"Ready - Loaded {len(self.dbg.ops)} ops")
        ttk.Label(toolbar, textvariable=self.status_var).pack(side=tk.RIGHT, padx=5)

        # --- Main Container ---
        self.main_container = ttk.Frame(self.root)
        self.main_container.pack(fill=tk.BOTH, expand=True, padx=5, pady=5)
        
        # Left Panel: Code
        code_frame = ttk.LabelFrame(self.main_container, text="Source Code")
        code_frame.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        
        self.code_list = tk.Listbox(code_frame, font=self.code_font, selectmode=tk.SINGLE)
        self.code_list.pack(fill=tk.BOTH, expand=True)
        
        scrollbar = ttk.Scrollbar(code_frame, orient=tk.VERTICAL, command=self.code_list.yview)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.code_list.config(yscrollcommand=scrollbar.set)
        
        # Batch Insert Code
        code_items = [str(op) for op in self.dbg.ops] # Simplification
        # Adding line numbers to listbox items
        list_items = [f"{i:04}: {op}" for i, op in enumerate(self.dbg.ops)]
        self.code_list.insert(tk.END, *list_items)
        
        # Right Panel: Memory
        mem_panel_frame = ttk.Frame(self.main_container)
        mem_panel_frame.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)
        
        self.mem_notebook = ttk.Notebook(mem_panel_frame)
        self.mem_notebook.pack(fill=tk.BOTH, expand=True)
        
        self.setup_memory_tabs()

    def setup_memory_tabs(self):
        # Register Tab
        if self.dbg.reg_start is not None:
             self.reg_frame = self.create_mem_grid(self.mem_notebook, "Registers", 
                                                   self.dbg.reg_start, 128)
             
        # Buffer Tab
        if self.dbg.buffer_start is not None:
             self.buf_frame = self.create_mem_grid(self.mem_notebook, "Buffer",
                                                   self.dbg.buffer_start, 32)
             
        # Local Tape Tab
        self.tape_frame = self.create_mem_grid(self.mem_notebook, "Tape (Local)", 0, 128)
        
        # Info Tab
        info_frame = ttk.Frame(self.mem_notebook)
        self.mem_notebook.add(info_frame, text="Info")
        info_text = f"""
        Registers Start: {self.dbg.reg_start}
        Buffer Start:    {self.dbg.buffer_start}
        Var Start:       {self.dbg.var_start}
        Stack Start:     {self.dbg.stack_start}
        """
        ttk.Label(info_frame, text=info_text, font=self.code_font).pack(padx=20, pady=20)

    def create_mem_grid(self, notebook, title, start_addr, count):
        frame = ttk.Frame(notebook)
        notebook.add(frame, text=title)
        
        # We will use a Canvas logic similar to before, but simplified
        canvas = tk.Canvas(frame)
        scrollbar = ttk.Scrollbar(frame, orient="vertical", command=canvas.yview)
        scrollable_frame = ttk.Frame(canvas)
        
        scrollable_frame.bind(
            "<Configure>",
            lambda e: canvas.configure(scrollregion=canvas.bbox("all"))
        )
        
        canvas.create_window((0, 0), window=scrollable_frame, anchor="nw")
        canvas.configure(yscrollcommand=scrollbar.set)
        
        canvas.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")
        
        # Store metadata for update
        frame.meta = {
            'container': scrollable_frame,
            'start': start_addr,
            'count': count,
            'widgets': {}
        }
        
        # Initial Population
        self.render_grid(frame, highlight_ptr=False)
        return frame

    def render_grid(self, frame, highlight_ptr=True):
        # Destroy old widgets 
        for w in frame.meta['container'].winfo_children():
            w.destroy()
            
        start = frame.meta['start']
        # For Local Tape
        if frame == getattr(self, 'tape_frame', None):
             window = 64
             start = max(0, self.dbg.ptr - window)
             start = (start // 8) * 8
        
        count = frame.meta['count']
        cols = 8
        
        for i in range(0, count, cols):
            row_addr = start + i
            
            # Row Label
            ttk.Label(frame.meta['container'], text=f"{row_addr:04}:", font=self.header_font).grid(row=i//cols, column=0, padx=5)
            
            for j in range(cols):
                if i + j >= count: break
                addr = row_addr + j
                
                if addr >= len(self.dbg.tape):
                    val = 0
                else:
                    val = self.dbg.tape[addr]
                
                # Default colors (system theme)
                bg = None
                fg = None
                
                # Highlight logic
                if highlight_ptr and addr == self.dbg.ptr:
                    bg = "blue"
                    fg = "white"
                elif val != 0:
                    bg = "#e0ffe0" # Light green
                    fg = "black"   # Force black on green
                
                # Note: tk.Label accepts None for bg/fg to use default
                # But to explicitly respect theme, it's better to NOT set them if None
                # However, tk.Label defaults are tricky.
                # Let's use simplified logic:
                kwargs = {}
                if bg: kwargs['bg'] = bg
                if fg: kwargs['fg'] = fg
                
                lbl = tk.Label(frame.meta['container'], text=f"{val:03}", font=self.mem_font, width=4, borderwidth=1, relief="solid", **kwargs)
                lbl.grid(row=i//cols, column=1+j, padx=1, pady=1)

    def update_view(self):
        # Code Highlight
        self.code_list.selection_clear(0, tk.END)
        self.code_list.selection_set(self.dbg.pc)
        self.code_list.see(self.dbg.pc)
        
        # Status
        self.status_var.set(f"Step: {self.dbg.step_count} | PC: {self.dbg.pc} | Ptr: {self.dbg.ptr}")
        
        # Update Active Tab Memory
        if self.mem_notebook.select():
            # Simply update all for robustness
            if hasattr(self, 'reg_frame'): self.render_grid(self.reg_frame)
            if hasattr(self, 'buf_frame'): self.render_grid(self.buf_frame)
            if hasattr(self, 'tape_frame'): self.render_grid(self.tape_frame)

        # Force UI update (Critical for macOS)
        self.root.update_idletasks()

    def step(self):
        if not self.dbg.run_step():
            self.is_running = False
            self.status_var.set("Finished")
            return False
        self.update_view()
        return True
    
    def toggle_run(self):
        if self.is_running:
            self.is_running = False
        else:
            self.is_running = True
            self.run_loop_tk()
            
    def run_loop_tk(self):
        if self.is_running:
            if self.step():
                # self.root.after(1, self.run_loop_tk) # fast
                # Let's throttle slightly to keep UI responsive
                self.root.after(5, self.run_loop_tk) 
            else:
                self.is_running = False
    
    def reset(self):
        self.is_running = False
        # To reset, we just re-parse or reload? simpler to re-create debugger
        # We can just reset dbg state if dbg supports it, but new Debugger() is safer
        # BUT we don't want to block main thread.
        # Quick hack: synchronous reload if code is already parsed, but parsing is fast?
        # Re-parsing 37000 ops is slow. Ideally debug class has reset.
        # We didn't impl reset in Debugger class, so let's just make new one in thread.
        
        self.mem_notebook.destroy()
        self.main_container.destroy()
        
        # Show loading again
        self.loading_frame.pack(fill=tk.BOTH, expand=True)
        self.progress.start()
        
        threading.Thread(target=self.load_data_thread, daemon=True).start()
        self.root.after(100, self.check_load_queue)

if __name__ == "__main__":
    root = tk.Tk()
    
    filename = None
    if len(sys.argv) >= 2:
        filename = sys.argv[1]
    else:
        # No arguments provided - open file dialog
        root.withdraw() # Hide the main window
        try:
            from tkinter import filedialog
            filename = filedialog.askopenfilename(
                title="Select Brainfuck File",
                filetypes=[("Brainfuck Files", "*.bf"), ("All Files", "*.*")]
            )
        except Exception as e:
            print(f"Error selecting file: {e}")
            sys.exit(1)
            
        if not filename:
            print("No file selected. Exiting.")
            root.destroy()
            sys.exit(0)
            
        root.deiconify() # Show the window again

    app = DebuggerGUI(root, filename)
    root.mainloop()
