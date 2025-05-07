from Windows_class import Windows, DEVC
from typing import List
import re

def generate_OBST_lines(win_obj:Windows, surf_id: str = 'Glass'):
    win_no = win_obj.no
    lines = []

    # Loop over height and width grid to generate OBSTs code
    for i in range(win_obj.height_cell_num + 1):
        for j in range(win_obj.width_cell_num + 1):
            if win_obj.thickness_orientation == 'y':
                # y is thickness direction --> sweep x
                x0 = win_obj.x_min + j * win_obj.GridSize
                x1 = x0 + win_obj.GridSize
                y0 = win_obj.y_min
                y1 = y0 + win_obj.thickness
            else:
                # X is thickness direction --> sweep y
                x0 = win_obj.x_min
                x1 = x0 + win_obj.thickness
                y0 = win_obj.y_min + j * win_obj.GridSize
                y1 = y0 + win_obj.GridSize
            
            # Always sweep Z
            z0 = win_obj.z_min + i * win_obj.GridSize
            z1 = z0 + win_obj.GridSize

            # Format the OBST line
            obst_id = f"WD{win_obj.no}_{i}_{j}"
            line = (
                f"&OBST ID='{obst_id}', "
                f"XB={x0:.2f},{x1:.2f},{y0:.2f},{y1:.2f},{z0:.2f},{z1:.2f} "
                f"SURF_ID='{surf_id}'"
                f","
                f"CTRL_ID='ControlWindowWD{win_obj.no}_{i}_{j}'"
                f"/\n"
            )

            lines.append(line)
    return lines
    
def insert_OBSTs_to_file(file_path: str, obst_lines: list[str], win_obj: Windows):
    # Read the txt
    with open(file_path, 'r', encoding='utf-8') as f:
        file_lines = f.readlines()

    # Locate line '----Windows----'
    insert_index = next((i + 1 for i, line in enumerate(file_lines) if '----Windows----' in line), None)
    if insert_index is None:
        print("Cant't find ----\"Windows\"---- index.")
        return

    # Insert section header
    header_line = f"--Window.no{win_obj.no}--\n"
    file_lines.insert(insert_index, header_line)
    insert_index += 1

    # Insert Generated window OBST line
    file_lines[insert_index:insert_index] = obst_lines

    # Write back to file
    with open(file_path, 'w', encoding='utf-8') as f:
        f.writelines(file_lines)


def generate_DEVC_lines(window: Windows, offset:float, offset_orientation:str, setpoint:int) -> List[str]:
    devc_lines = []

    for i in range(window.height_cell_num + 1):
        for j in range(window.width_cell_num + 1):
            # Calculate OBST bounding box
            if window.thickness_orientation == 'y':
                x0 = window.x_min + j * window.GridSize
                x1 = x0 + window.GridSize
                y0 = window.y_min
                y1 = y0 + window.thickness
            else:
                x0 = window.x_min
                x1 = x0 + window.thickness
                y0 = window.y_min + j * window.GridSize
                y1 = y0 + window.GridSize
            
            z0 = window.z_min + i * window.GridSize
            z1 = z0 + window.GridSize

            # Extract the center
            cx = (x0 + x1) / 2
            cy = (y0 + y1) / 2
            cz = (z0 + z1) / 2

            # Apply offset
            if offset_orientation.lower() == 'x':
                cx += offset
            elif offset_orientation.lower() == 'y':
                cy += offset
            elif offset_orientation.lower() == 'z':
                cz += offset
            else:
                raise ValueError("offset_orientation must be 'x', 'y', or 'z'")
            
            # Custom ID format: DEVC-WDno_x_x
            devc_id = f"DEVC-WD{window.no}_{i}_{j}"

            # Generate line
            devc_line = (
                f"&DEVC ID='{devc_id}', "
                f"QUANTITY='TEMPERATURE', "
                f"XYZ={cx:.2f},{cy:.2f},{cz:.2f}, "
                f"ORIENTATION=0.0,-1.0,0.0, " 
                f"SETPOINT={setpoint}/\n"
            )
            devc_lines.append(devc_line)
    
    return devc_lines


def insert_DEVCs_to_file(file_path:str, devc_lines:list[str], win:Windows):
    window_no = win.no
    with open(file_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    insert_index = next((i + 1 for i, line in enumerate(lines) if '----DEVC----' in line), None)
    if insert_index is None:
        print("Can't find ----\"DEVC\"---- marker.")
        return

    # Insert section header
    header = f"--DEVC.Window.no{window_no}--\n"
    lines.insert(insert_index, header)
    insert_index += 1

    # Insert all DEVC lines
    lines[insert_index:insert_index] = devc_lines

    with open(file_path, 'w', encoding='utf-8') as f:
        f.writelines(lines)


def generate_CTRL_lines(window:Windows):
    win_no = window.no
    ctrl_lines = []

    for i in range(window.height_cell_num + 1):
        for j in range(window.width_cell_num + 1):
            ctrl_id = f"ControlWindowWD{window.no}_{i}_{j}"
            latch_id = f"latch{window.no}_{i}_{j}"
            line_1 = (
                f"&CTRL ID='{ctrl_id}', "
                f"FUNCTION_TYPE='ALL', "
                f"LATCH=.FALSE., "
                f"INITIAL_STATE=.TRUE., "
                f"INPUT_ID='{latch_id}'/"
                f"\n"
            )

            ctrl_lines.append(line_1)

            line_2 = (
                f"&CTRL ID='{latch_id}', "
                f"FUNCTION_TYPE='ALL', "
                f"LATCH=.TRUE., "
                f"INPUT_ID='DEVC-WD{window.no}_{i}_{j}'/"
                f"\n"
            )

            ctrl_lines.append(line_2)
    
    return ctrl_lines


def insert_CTRLs_to_file(file_path:str, ctrl_lines:list[str], win:Windows):
    window_no = win.no
    with open(file_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    insert_index = next((i + 1 for i, line in enumerate(lines) if '----CTRL----' in line), None)
    if insert_index is None:
        print("Can't find ----\"DEVC\"---- marker.")
        return

    # Insert section header
    header = f"--CTRL.Window.no{window_no}--\n"
    lines.insert(insert_index, header)
    insert_index += 1

    # Insert all DEVC lines
    lines[insert_index:insert_index] = ctrl_lines

    with open(file_path, 'w', encoding='utf-8') as f:
        f.writelines(lines)







