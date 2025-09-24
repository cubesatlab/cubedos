# Setup

## Install Ada/Spark tools

- Run `CubeSatSetup.sh`
	- **NOTE:** this requires credentials to the CubeSat resources on lemuria
    - Recommend running somewhere like a `Downloads` folder, where you don't mind having all the files it downloads dumped.
	- Copy (or preserve by some other method) the paths displayed at the end of the script, as you will need them for environment configuration.
- Path/Environment Setup (will depend on what shell you use):
	- bash (add lines to the tail end of your `.bashrc`)
		- `export PATH=/paths/to/install/dirs:/separated/by/colons:$PATH`
		- `export GPR_PROJECT_PATH=/path/to/install/dir:$GPR_PROJECT_PATH`
		- `export LIBRARY_PATH=/path/to/install/dir:$LIBRARY_PATH`
		- `export LD_LIBRARY_PATH=/path/to/install/dir:$LD_LIBRARY_PATH`
	- zsh (add lines to the tail end of your `.zshrc`)
		- `path=('/path/to/tool/bin' $path)`
			- Repeat the above for each tool (gnatdas, gnatsas, gnatpro, etc.)
		- `GPR_PROJECT_PATH=/path/to/install/dir:$GPR_PROJECT_PATH`
		- `LIBRARY_PATH=/path/to/install/dir:$LIBRARY_PATH`
		- `LD_LIBRARY_PATH=/path/to/install/dir:$LD_LIBRARY_PATH`
		- `export GPR_PROJECT_PATH LIBRARY_PATH LD_LIBRARY_PATH PATH`


## Using gnatstudio in WSL

Once the installation above has been completed, you can simply call `gnatstudio &` and it will pop up in a new window.


## VS Code Path/Env configuration
- Open the appropriate `settings.json` file
	- `Ctrl + Shift + p` opens the command palette
		- `Preferences: Open User Settings (JSON)` will edit your overall settings
		- `Preferences: Open Workspace Settings (JSON)` will edit settings for the current workspace (This is probably less desirable, as these settings are stored in the workspace, potentially tracked in git, and the options we're configuring may vary from setup to setup)
- Using the Path/Directory info output by `CubeSatSetup.sh`, fill out the following template and add it to the `settings.json` file.
	- Replace `<install_dir>` with wherever the script indicates your Ada/Spark tools were installed.

```json
	"terminal.integrated.env.linux": {
        "PATH": "<install_dir>/gnatpro/bin:<install_dir>/spark/bin:<install_dir>/gnatdas/bin:<install_dir>/gnatsas/bin:${env:PATH}",
        "GPR_PROJECT_PATH": "<install_dir>/libadalang/share/gpr:${env:GPR_PROJECT_PATH}",
        "LIBRARY_PATH": "<install_dir>/libadalang/lib:${env:LIBRARY_PATH}",
        "LD_LIBRARY_PATH": "<install_dir>/libadalang/lib:${env:LD_LIBRARY_PATH}",
    }
```


### Adding custom build tasks

There is a section in the VS Code documentation on creating [build tasks](https://code.visualstudio.com/docs/editor/tasks). You may also find some of the information in the AdaCore [getting started documentation](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started) useful.

#### TODO: Create a set of reference build tasks for the CubedOS project


### Known Issues

- Automatic detection/configuration of build paths is very temperamental
	- `Run` or `Debug` buttons may not populate above Main procedures
	- Sometimes, the buttons will populate, but using them will give an error along the lines of: `gprbuild: "src/check/main.adb" was not found in the sources of any project`
		- This issue seems to be an issue with the automatic path population generating bad relative paths based on the workspace.
		- This is likely fixable, but I haven't yet found a consistent solution. Based on what I'm seeing, my best guess is that the AdaCore extension for VS Code is struggling with the project layout, possibly as a result of there being multiple projects in a single repository.
		- For the `adatutorials` project, this only seems to occur when you say yes to the popup asking "Some project source directories are not listed in your workspace: do you want to add them?" (It appears to pull the `aunit` directory into the workspace, which we shouldn't need, as we shouldn't be manipulating those files)
