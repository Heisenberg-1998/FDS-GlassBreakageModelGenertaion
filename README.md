#  Automatic FDS Code Generator for Glass Breakage Model
This is a automotive FDS code generate programme which can help you to generate a ‘criterion-controlled’ glass breakage model proposed from Chu et al (2024)

In the paper ***Integrating glass breakage models into CFD simulation to investigate realistic compartment fire behaviour*** (Chu et al., 2024), a ‘criterion-controlled’ glass breakage model was proposed as follow:
![image](https://github.com/user-attachments/assets/16c3099d-121c-4bbf-b0e8-1d7cce284fd1)

# How to use the Code Generator
Before strating your own trail, please ensure the folder contain a `window.txt` file, which has following text format:
```txt
----Windows----

----DEVC----

----CTRL----
```
<p align="center">
  <img src="https://github.com/user-attachments/assets/9220cd60-00ff-4fd3-abc3-38e85dc052cf" width="400" />
</p>

`main.py` privided three examples, three window instances are initialized according to the `window.no`, `x_min`, `y_min`, `z_min`, `x_max`, `y_max`, `z_max`, `thickness` and `GridSize`.
```python
    win_no1 = Windows(no=1, 
                      x_min=1.0, x_max=2.4, 
                      y_min=4.4, y_max=4.5, 
                      z_min=1.2, z_max=2.4,
                      thickness = 0.1, 
                      GridSize=0.1)

    win_no2 = Windows(no=2, 
                      x_min=0.0, x_max=2.4, 
                      y_min=-0.2, y_max=0.0, 
                      z_min=1.2, z_max=2.4,
                      thickness = 0.1, 
                      GridSize=0.1)

    win_no3 = Windows(no=3, 
                      x_min=-0.2, x_max=0, 
                      y_min=-0.2, y_max=2.2, 
                      z_min=1.2, z_max=2.4,
                      thickness = 0.1, 
                      GridSize=0.1)
```

Then use function `generate_OBST_lines`, `generate_DEVC_lines`, `generate_CTRL_lines` to generate OBST, DEVC and CTRL code:

***Note 1. In `generate_DEVC_lines(window: Windows, offset:float, offset_orientation:str, setpoint:int)`, `offset` is the distance betwwen DEVC (thermocouple or heat flux meter) and windows cells***

***Note 2. In `generate_OBST_lines(win_obj:Windows, surf_id: str = 'Glass')`, `surf_id` is the glass's surface id FDS, you should modify the text `Glass` according to your own FDS model***
```
    obst_line = Edit_txt_file.generate_OBST_lines(win_obj=win_no1)
    devc_line = Edit_txt_file.generate_DEVC_lines(window=win_no1, offset=-0.1, offset_orientation='y', setpoint=150)
    ctrl_line = Edit_txt_file.generate_CTRL_lines(window=win_no1)
    
```

Then use function `insert_OBSTs_to_file`, `insert_DEVCs_to_file`, `insert_CTRLs_to_file` to insert `obst_line`, `devc_line`, `ctrl_line` to the txt file `window_txt`.
```
  Edit_txt_file.insert_OBSTs_to_file(file_path,obst_line, win_no1)
  Edit_txt_file.insert_DEVCs_to_file(file_path, devc_line, win_no1)
  Edit_txt_file.insert_CTRLs_to_file(file_path, ctrl_line, win_no1)
```

After runing `main.py`, you can find FDS code in `window.txt`, then you can copy-paste the text to your own FDS file:
```txt
----Windows----
--Window.no3--
&OBST ID='WD3_0_0', XB=-0.20,-0.10,-0.20,-0.10,1.20,1.30 SURF_ID='Glass',CTRL_ID='ControlWindowWD3_0_0'/
&OBST ID='WD3_0_1', XB=-0.20,-0.10,-0.10,0.00,1.20,1.30 SURF_ID='Glass',CTRL_ID='ControlWindowWD3_0_1'/
&OBST ID='WD3_0_2', XB=-0.20,-0.10,0.00,0.10,1.20,1.30 SURF_ID='Glass',CTRL_ID='ControlWindowWD3_0_2'/
...
--Window.no2--
&OBST ID='WD2_0_0', XB=0.00,0.10,-0.20,-0.10,1.20,1.30 SURF_ID='Glass',CTRL_ID='ControlWindowWD2_0_0'/
&OBST ID='WD2_0_1', XB=0.10,0.20,-0.20,-0.10,1.20,1.30 SURF_ID='Glass',CTRL_ID='ControlWindowWD2_0_1'/
...
--Window.no1--
...

----DEVC----
--DEVC.Window.no3--
&DEVC ID='DEVC-WD3_0_0', QUANTITY='TEMPERATURE', XYZ=-0.05,-0.15,1.25, ORIENTATION=0.0,-1.0,0.0, SETPOINT=150/
&DEVC ID='DEVC-WD3_0_1', QUANTITY='TEMPERATURE', XYZ=-0.05,-0.05,1.25, ORIENTATION=0.0,-1.0,0.0, SETPOINT=150/
...
--DEVC.Window.no2--
...
--DEVC.Window.no1--
...

----CTRL----
--CTRL.Window.no3--
&CTRL ID='ControlWindowWD3_0_0', FUNCTION_TYPE='ALL', LATCH=.FALSE., INITIAL_STATE=.TRUE., INPUT_ID='latch3_0_0'/
&CTRL ID='latch3_0_0', FUNCTION_TYPE='ALL', LATCH=.TRUE., INPUT_ID='DEVC-WD3_0_0'/
&CTRL ID='ControlWindowWD3_0_1', FUNCTION_TYPE='ALL', LATCH=.FALSE., INITIAL_STATE=.TRUE., INPUT_ID='latch3_0_1'/
&CTRL ID='latch3_0_1', FUNCTION_TYPE='ALL', LATCH=.TRUE., INPUT_ID='DEVC-WD3_0_1'/
```
The FDS result is shown as follow:
<p align="center">
  <img src="https://github.com/user-attachments/assets/f7fd08e8-0082-4bc5-88da-59205fe83937" width="400" />
</p>

# What to do next...
The breakage is not only controlled by the temperture but also by the heat flux, add heat flux controlled function in next stage...

# Reference
Chu, T., Jiang, L., Zhu, G., Usmani, A., 2024. Integrating glass breakage models into CFD simulation to investigate realistic compartment fire behaviour. Journal of Building Engineering 82, 108314. https://doi.org/10.1016/j.jobe.2023.108314
