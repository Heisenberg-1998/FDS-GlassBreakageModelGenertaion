import math

class Windows:
    def __init__(
            self, 
            no:int,
            x_min:float,
            x_max:float, 
            y_min:float, 
            y_max:float, 
            z_min:float, 
            z_max:float, 
            thickness:float,
            GridSize:float
    ):
        
        # Windows index in FDS
        self.no = int(no)

        # Window Geometry
        self.x_min = x_min
        self.x_max = x_max
        self.y_min = y_min
        self.y_max = y_max
        self.z_min = z_min
        self.z_max = z_max

        self.thickness = thickness
        self.GridSize = GridSize

        # Compute window dimensions
        x_range = x_max - x_min
        y_range = y_max - y_min
        self.width = max(x_range, y_range)
        self.height = z_max - z_min

        # Find the thickness orientation
        self.thickness_orientation = 'y' if x_range > y_range else 'x'
        
        epsilon = 1e-6
        if abs((self.width / self.GridSize) - round(self.width / self.GridSize)) > epsilon or \
        abs((self.height / self.GridSize) - round(self.height / self.GridSize)) > epsilon:
            raise ValueError(
                f"GridSize = {self.GridSize} does not divide evenly with width={self.width} or height={self.height}"
            )

        # Calculate number of grid cells in each direction
        self.width_cell_num = int(self.width / self.GridSize)
        self.height_cell_num = int(self.height / self.GridSize)

        # Total number of grid cells
        self.total_cell_num = self.width_cell_num * self.height_cell_num

        # Debug function
    def __repr__(self):
        return (
            f"<Window no={self.no}, thickness_dir='{self.thickness_orientation}', "
            f"size=({self.width}, {self.height}), grid_num=({self.width_cell_num}x{self.height_cell_num})>"
        )


class DEVC:
    def __init__(self, 
                 x:float, y:float, z:float, 
                 offset:float, offset_orientation:str, 
                 SETPOINT:int):
        self.x = x
        self.y = y
        self.z = z
        self.SETPOINT = SETPOINT
        self.offset = offset
        self.offset_orientation = offset_orientation.lower()

        # Apply offset based on orientation
        if self.offset_orientation == 'x':
            self.x = x + offset
            self.y = y
            self.z = z
        elif self.offset_orientation == 'y':
            self.x = x
            self.y = y + offset
            self.z = z
        elif self.offset_orientation == 'z':
            self.x = x
            self.y = y
            self.z = z + offset
        else:
            raise ValueError("Offset_orientation must be 'x', 'y', 'z'")
        
    def __repr__(self):
        return f"<DEVC x={self.x}, y={self.y}, z={self.z}, SETPOINT={self.SETPOINT}>"

    def to_fds(self, id: str = 'Detector') -> str:
        return f"&DEVC ID='{id}', XYZ={self.x:.2f},{self.y:.2f},{self.z:.2f}, SETPOINT={self.SETPOINT}/\n"


