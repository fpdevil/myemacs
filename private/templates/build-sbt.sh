#!/usr/bin/env bash

#==============================================================================
# File Name    : build-sbt.sh
# Author       : Sampath Singamsetty
# mail         : Singamsetty.Sampath@gmail.com
# Created Time : Mon Jan  1 20:49:16 2018
# Copyright (C) 2018 Sampath Singamsetty
#==============================================================================
cwd="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

project_name=${1:-sbt_eclipse_project}

project_version=${2:-1.0}

scala_version=${3:-2.11.7}

sbt_version=${4:-0.13.9}

build_file=project.sbt

project_dir=project

# Project sources
sources=(src/main/scala src/main/resources src/test/scala src/test/resources)

# Create the project directory
mkdir $project_name && cd $_

# Add source directories
for s in ${sources[@]}; do
  mkdir -p "$s"
done

# Add the build file
echo "name := \"$project_name\""$'\n'"version := \"$project_version\""$'\n'"scalaVersion := \"$scala_version\"" >> $build_file

# Create project configuration (including plugins)
mkdir $project_dir
echo "sbt.version=$sbt_version" >> $project_dir/build.properties
echo "addSbtPlugin(\"com.typesafe.sbteclipse\" % \"sbteclipse-plugin\" % \"4.0.0\")" >> $project_dir/plugins.sbt

# Finally, build the project
cd $cwd/$project_name && sbt eclipse

exit 0
