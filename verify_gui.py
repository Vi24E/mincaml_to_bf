import tkinter as tk
import sys
import threading
import time
from debugger_gui import DebuggerGUI

def dump_widget_hierarchy(widget, indent=0):
    try:
        w_id = widget.winfo_id()
        w_class = widget.winfo_class()
        w_width = widget.winfo_width()
        w_height = widget.winfo_height()
        w_x = widget.winfo_x()
        w_y = widget.winfo_y()
        w_mapped = widget.winfo_ismapped()
        print(f"{' ' * indent} {w_class} ({w_width}x{w_height} at {w_x},{w_y}) Mapped={w_mapped}")
    except Exception as e:
        print(f"{' ' * indent} Error inspecting {widget}: {e}")

    for child in widget.winfo_children():
        dump_widget_hierarchy(child, indent + 2)

def verify():
    print("--- Starting Verification ---")
    root = tk.Tk()
    # Force a size
    root.geometry("800x600")
    
    app = DebuggerGUI(root, "debug.bf")
    
    def inspect():
        print("Waiting for UI to load...")
        time.sleep(10)
        print("\n--- Widget Hierarchy Dump ---")
        dump_widget_hierarchy(root)
        print("--- End Dump ---")
        root.quit()
        
    threading.Thread(target=inspect, daemon=True).start()
    
    try:
        root.mainloop()
    except Exception as e:
        print(f"Mainloop error: {e}")

if __name__ == "__main__":
    verify()
