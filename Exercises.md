# Exercises

The tutorial is structured in exercises of increasing involvement. The initial
tasks, that deal with the setting up of the working environment, should
preferably be carried out before the start of the tutorial.

## 0. Setup of the working environment

Firstly, we will set up a plain ROS2 environment. With this, we would be ready
to develop ROS2 packages in C, C++ or Python. Subsequently, we will set up the
Ada-specific environment.

Remember that this tutorial has been designed to be carried out on Ubuntu 20.04 LTS.

### ROS2 setup

The simplest way of setting up ROS2 in Ubuntu is from binary packages. Follow
the instructions at
https://docs.ros.org/en/foxy/Installation/Ubuntu-Install-Debians.html, which are
summarized below:

1. Ensure your system locale supports UTF-8 (`locale` command).
1. Install pre-requisite packages:
    1. `sudo apt update && sudo apt install curl gnupg2 lsb-release`
1. Set up the ROS2 keys and apt origin:
    1. `sudo curl -sSL https://raw.githubusercontent.com/ros/rosdistro/master/ros.key  -o /usr/share/keyrings/ros-archive-keyring.gpg`
    1. `echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/ros-archive-keyring.gpg] http://packages.ros.org/ros2/ubuntu $(lsb_release -cs) main" | sudo tee /etc/apt/sources.list.d/ros2.list > /dev/null`
1. Install the ROS2 packages:
    1. `sudo apt update`
    1. `sudo apt install ros-foxy-desktop python3-argcomplete`

Our shell is unaware of ROS2 tools until we activate some ROS version. This is
done by sourcing the `/opt/ros/<version>/setup.bash` file (a `zsh` alternative exists too):

> `source /opt/ros/foxy/setup.bash`

Afterwards, we can verify our system is ready running, for example, `ros2`.

The `ros-foxy-desktop` package will install a complete suite of packages that
also include demos and tutorials. We are going to use some of these, but
otherwise a more limited set of packages could be installed with
`ros-foxy-ros-base`.

ROS/ROS2 distributions use packages with the prefix `ros-<version>-*`. You may see other ROS2 versions with their base Ubuntu system and support life at https://docs.ros.org/en/rolling/Releases.html. 

ROS Foxy, which is used in this tutorial, is the current Long Term Service (LTS) ROS2 distribution. Very recently the next non-LTS version was released. Also, since June 2020, a rolling distribution exists on which development is carried out. RCLAda is developed on the latest LTS release.

### A note on terminals

Development for ROS/ROS2 tends to require multiple commands run from several
terminals. For this reason, it is advisable to use a terminal that allows
splitting it into sub-terminals. A graphical popular option is `terminator`. An
alternative that will work in most consoles, even non-graphical ones, is `tmux`,
although this one is a bit more complex to learn as it requires learning
shortcuts. (See, e.g., https://tmuxcheatsheet.com/, in particular the section
about *"panes"*.)

Likewise, a terminal that can be called/dismissed quickly may result convenient
to some people. A personal favorite (which also supports splitting) is `guake`,
bound to some unused function key (e.g. `F12`).

### RCLAda setup

With the previous steps, and after sourcing the `setup.bash` script, we are ready to set up the Ada environment. Full instructions can be found at https://github.com/ada-ros/ada4ros2 and are summarized next:

1. Install the native Ada build tools:
    1. `apt install gnat gnat-gps gprbuild`
    - GNAT CE 2020 will not work as it will complain about a few points in the Ada codebase. 
    - Even after fixing those, you may experience linking problems when mixing code built with the native `g++` and the one packaged with GNAT CE 2020. 
    - You **may** use the GNATstudio editor from CE 2020, if so you prefer, over the older GPS packaged in ubuntu as `gnat-gps`.
1. Clone the RCLAda sources, including submodules, and using the aeic21 branch:
    1. `git clone --recurse-submodules -b aeic21 https://github.com/ada-ros/ada4ros2`
    - The `ada4ros2` is a mostly empty repository, that is used to bring in several ROS2 packages as submodules. It also contains a few convenience scripts, but nothing necessary *per se* to use RCLAda. 
    - The Ada ROS2 packages are detailed in the first part of the tutorial presentation. You can find them under the `src` folder of the repository.
1. Enter the cloned repository: 
    1. `cd ada4ros2`
1. Ensure the ROS2 environment is loaded: `source /opt/ros/foxy/setup.bash`
    - There is no ill effect by sourcing the script several times.
1. Build the Ada for ROS2 packages:
    1. `colcon build`
    - `colcon` is the build tool used by ROS2. It is an 'orchestrator' of the build, but it does not mandate a particular method for building a package. It determines dependencies and dispatches to the build method of each package.
    - The build should finish without errors; otherwise we cannot continue.
1. Load the new environment that includes the just-compiled Ada packages:
    1. `source install/setup.bash`
    - ROS2 environments are properly layered, so when the `install/setup.bash` script is generated by the build process, it also will load the pre-existing environment; in this case, the base ROS2 environment. This means that, in a new terminal, it is enough to source the `ada4ros2/install/setup.bash` script to be ready to go.
1. Verify the build succeeded by running, for example:
    1. `ros2 run rclada rclada_selftest_dynamic`
    - Although there may be an error message about a timer, as long as no exceptions are raised, everything is fine.
    - The run should end with a report on allocated memory similar to this one:

```
Total allocated bytes :  425
Total logically deallocated bytes :  425
Total physically deallocated bytes :  0
Current Water Mark:  0
High Water Mark:  425
```

If you experience difficulties setting up the environment, or do not have an Ubuntu base system, there are a couple of alternatives you may try. These alternatives are described next.

### Alternatives: gitpod

GitPod is a remote development environment that allows to run VSCode in a browser, with a particular environment generated with Docker underneath. This approach serves for the first exercises, that do not require visualization. For this reason, it is recommended as a last resort for this tutorial, or as a temporary measure while the previous instructions are completed.

The gitpod service is free for open source projects, but it requires an account on Github/Gitlab/Bitbucket.

Launch the prepared gitpod environment by following this link:

> https://gitpod.io/#https://github.com/ada-ros/ada4ros2/tree/feat/tut-21

An error in the terminal of VSCode like this:

```
ls: cannot access '/home/gitpod/.bashrc.d/*': No such file or directory
```

comes from the own Gitpod service and is inoffensive.

The Gitpod session already has ROS2 preinstalled, and the underlying Docker is an Ubuntu 20.04. Thus, it is enough to do the following in the VSCode terminal to catch up to the end of the previous section:

1. `source /opt/ros/foxy/setup.bash`
2. `colcon build`

### Alternatives: docker

A more powerful alternative is to use a Docker image that already contains the ROS2 environment. This may allow to successfully follow the tutorial in another Linux other than Ubuntu 20.04. The image is: <TODO: NAME OF IMAGE>

This image is configured to connect to the X server of the host machine (so it working with Wayland is unclear). You need to additionally connect your source directory so it is accessible within the Docker. A script is provided for your convenience that you can inspect for details:

`dev/docker.sh`

Running this script will open a terminator terminal from within the docker, with the ROS2 environment already loaded. (No need to source the `/opt/ros/foxy/setup.bash` script.)

## 1. Creating a new ROS2 package for Ada development



## 2. Blackboard communication (topics/publishers/subscriptions)

### Using RCLAda dynamic messages

TODO: explanation (not actually an exercise)

### Using RCLAda static messages

TODO: explanation (not actually an exercise)

### Writing a Publisher, dynamic version

publisher_dynamic.adb

Verify with `ros2 topic echo <topic>`

Also `ros2 topic {info|type}`

### Writing a Publisher, static version

publisher_static.adb

### Writing a Subscriber, dynamic version

subscriber_dynamic.adb

$ ros2 topic pub /chatter std_msgs/msg/String 'data: "hello"'

See that autocompletion works for ros2 topic etc.

### Writing a Subscriber, static version

### TurtleSim: first steps

### ePuck: first steps

### Pioneer 3AT: first steps



## 3. PRC communication (services/servers/clients)

### Servers

### Asynchronous clients

### Synchronous clients



## 4. Realistic exercises in the local frame

### Wandering robot with obstacle avoidance (Roomba-like)

### Navigate to a local goal (VFH local planner)



## 5. Realistic exercises in the global frame

###  Navigate to a world goal (transform + VFH)


## 6. Defining your own messages