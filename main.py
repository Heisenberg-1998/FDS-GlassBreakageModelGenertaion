import Edit_txt_file
import Windows_class
from Windows_class import Windows

def main():
    file_path = 'windows.txt'
    
    win_no1 = Windows(no=1, 
                      x_min=1.0, x_max=2.4, 
                      y_min=4.4, y_max=4.5, 
                      z_min=1.2, z_max=2.4,
                      thickness = 0.1, 
                      GridSize=0.1)
    
    print(win_no1.__repr__())
    
    obst_line = Edit_txt_file.generate_OBST_lines(win_obj=win_no1)
    Edit_txt_file.insert_OBSTs_to_file(file_path,obst_line, win_no1)

    devc_line = Edit_txt_file.generate_DEVC_lines(window=win_no1, offset=-0.1, offset_orientation='y', setpoint=150)
    Edit_txt_file.insert_DEVCs_to_file(file_path, devc_line, win_no1)

    ctrl_line = Edit_txt_file.generate_CTRL_lines(window=win_no1)
    Edit_txt_file.insert_CTRLs_to_file(file_path, ctrl_line, win_no1)
    

    win_no2 = Windows(no=2, 
                      x_min=0.0, x_max=2.4, 
                      y_min=-0.2, y_max=0.0, 
                      z_min=1.2, z_max=2.4,
                      thickness = 0.1, 
                      GridSize=0.1)
    print(win_no2.__repr__())
    obst2_line = Edit_txt_file.generate_OBST_lines(win_obj=win_no2)
    Edit_txt_file.insert_OBSTs_to_file(file_path,obst2_line, win_no2)

    devc2_line = Edit_txt_file.generate_DEVC_lines(window=win_no2, offset=0.1, offset_orientation='y', setpoint=150)
    Edit_txt_file.insert_DEVCs_to_file(file_path, devc2_line, win_no2)

    ctrl2_line = Edit_txt_file.generate_CTRL_lines(window=win_no2)
    Edit_txt_file.insert_CTRLs_to_file(file_path, ctrl2_line, win_no2)

    win_no3 = Windows(no=3, 
                      x_min=-0.2, x_max=0, 
                      y_min=-0.2, y_max=2.2, 
                      z_min=1.2, z_max=2.4,
                      thickness = 0.1, 
                      GridSize=0.1)
    print(win_no3.__repr__())
    obst3_line = Edit_txt_file.generate_OBST_lines(win_obj=win_no3)
    Edit_txt_file.insert_OBSTs_to_file(file_path,obst3_line, win_no3)

    devc3_line = Edit_txt_file.generate_DEVC_lines(window=win_no3, offset=0.1, offset_orientation='x', setpoint=150)
    Edit_txt_file.insert_DEVCs_to_file(file_path, devc3_line, win_no3)

    ctrl3_line = Edit_txt_file.generate_CTRL_lines(window=win_no3)
    Edit_txt_file.insert_CTRLs_to_file(file_path, ctrl3_line, win_no3)



if __name__ == "__main__":
    main()