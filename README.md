# Programming mobile robots with ROS2 and the RCLAda Client Library

A tutorial

## Preparation

The tutorial includes a number of practical exercises. They have been developed and tested on: 

- Ubuntu 20.04. 
    - Downloadable from https://ubuntu.com/download/desktop 
- ROS2 version Foxy.
    - Instructions at https://docs.ros.org/en/foxy/Installation/Ubuntu-Install-Debians.html
    - Install package `ros-foxy-desktop`
- GNAT from the Ubuntu distribution (package gnat, gnatls -v output is version 9.3)
- GNATStudio from GNAT Community Edition 2020
    - The package `gnat-gps` from Ubuntu might work too, but this is untested.
- Webots simulator from the ROS2 Ubuntu packages.
    - Install package `ros-foxy-webots-ros2`
    - Run once the command `ros2 launch webots_ros2_epuck robot_launch.py` to
      trigger the actual installation of the simulator.

To be able to carry out the exercises with minimal friction, it is recommended to have a computer with the given configuration. Other Linux distributions might work with unknown amounts of tinkering, by installing ROS2 from sources, but this might be troublesome and without guarantees of success. Windows will not work as RCLAda does not target it.

### Hardware requirements

For some exercises the *Webots* simulator will be used. This simulator is not very demanding about hardware or graphics acceleration, working well even with recent Intel integrated GPUs. A computer well-suited for large compilations is recommended, though.

