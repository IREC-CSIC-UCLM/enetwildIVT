# enetwildIVT

enetwildIVT is an R package that contain a golemized shinyapp to validate the harmonized databases provided by the partners of the ENEWTILD proyect.

## Installation

To install locally the Dockerized ShinyApp go to the releases section on this repository and download from the latest one:

- .tar.gz file containing the shinyapp as an Rpackage
- Dockerfile for dependencies
- README with commands

## Usage

To use this docker image you first need to have installed docker, here you can find more information regarding Docker instalation: https://docs.docker.com/engine/install/

Using your terminal, go to the directory where you have your three downloaded files.

```sh
cd your/directory/path
```

Once you are there, build the enetwildIVT image using this command:

```sh
docker build -t enetwildivt
```

To run the shinyapp just execute this docker run command: 

```sh
docker run -p 3838:3838 -v /opt/enetwildIVT:/opt/enetwildIVT enetwildivt:latest
```

## Issues

Please if you find issues report them on: https://github.com/IREC-CSIC-UCLM/enetwildIVT/issues

## License

This package is licensed under the MIT License. See the LICENSE file for details.
