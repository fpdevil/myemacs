#!/bin/bash
# shell script for switching between Aquamacs and Spacemacs
# the script should be run from the users home directory
#
# Aquamacs configuration file is aquamacs
#          configuration directory is aquamacs.d
# Spacemacs configuration file is spacemacs
#           configuration directory is spacemacs.d
# Prelude configuration file is prelude
#           configuration directory is prelude.d
#
# the script is executed with an option A or S for
# switching to either Aquamacs or Spacemacs by creating
# softlinks to appropriate files and directories
#
# This script should be executed from users HOME directory
# ./switcher.sh S | ./switcher.sh A | ./switcher.sh P
#
# Base files and directories for each
# Aquamacs  - ~/aquamacs & ~/aquamacs.d
# Spacemacs - ~/spacemacs & ~/spacemacs.d
# Prelude   - ~/prelude * ~/prelude.d
############################################################

usage()
{
    echo "Usage $0 <editor code>"
    echo "provide an editor code to switch..."
    echo "@ A - aquamacs"
    echo "@ S - spacemacs"
    echo "@ P - prelude"
    return 1
}

A=Aquamacs
S=Spacemacs
P=Prelude
# if no input is provided show usage information
if [ $# -eq 0  ]
then
    usage
    exit $?
else
    # check for option aquamacs
    if [[ $1 == A ]]
    then
        aq=aquamacs
        aqd=aquamacs.d
        # if [[ -L .emacs  &&  -L .emacs.d ]]
        if [[ -L .emacs  &&  -L .emacs.d ]] && [[ $(readlink -n .emacs.d) == $aqd  && $(readlink -n .emacs) == $aq ]]
        then
            echo "Option $1 and Already at $A"
            exit $?
        else
            if [[ -f aquamacs && -d aquamacs.d ]]
            then
                echo "switching to $A for option $1"
                echo "clearing and creating soft links for aquamacs..."
                # remove any existing links to spacemacs
                rm -rvf .spacemacs .emacs .emacs.d
                # I am cleaning up my old customizations which can be
                # commented out if not needed.
                rm -rvf aquamacs.d/SessionDesktop.el aquamacs.d/custom-settings.el
                rm -vrf ~/Library/Application\ Support/Aquamacs\ Emacs/*
                rm -vrf ~/Library/Preferences/Aquamacs\ Emacs/*
                ln -s aquamacs .emacs
                ln -s aquamacs.d .emacs.d
                echo "------------------------------------------------------------"
                exit $?
            else
                echo "aquamacs directories does not exist for $A and $1 change"
                echo $?
            fi
        fi
    # check for option spacemacs
    elif [[ $1 == S ]]
    then
        if [[ -L .spacemacs && -L .emacs.d ]]
        then
            echo "Already at $S for $1"
            exit $?
        else
            if [[ -f spacemacs && -d spacemacs.d ]]
            then
                echo "switching to $S for option $1"
                echo "creating soft links for spacemacs..."
                # remove any existing links to aquamacs
                rm -vrf .emacs .emacs.d
                ln -s spacemacs .spacemacs
                ln -s spacemacs.d .emacs.d
                exit $?
            else
                echo "spacemacs directories does not exist for $S and $1 change"
                exit $?
            fi
        fi
    # check for option prelude
    elif [[ $1 == P ]]
    then
        pl=prelude
        pld=prelude.d
        # if [[ -L .emacs  &&  -L .emacs.d ]]
        if [[ -L .emacs  &&  -L .emacs.d ]] && [[ $(readlink -n .emacs.d) == $pld  && $(readlink -n .emacs) == $pl ]]
        then
            echo "Option $1 and Already at $P"
            exit $?
        else
            if [[ -f prelude && -d prelude.d ]]
            then
                echo "switching to $P for option $1"
                echo "creating soft links for prelude..."
                # remove any existing links to aquamacs
                rm -vrf .spacemacs .emacs .emacs.d
                ln -s prelude .emacs
                ln -s prelude.d .emacs.d
                exit $?
            else
                echo "prelude directories does not exist for $P and $1 change"
                exit $?
            fi
        fi
    else
        echo "invalid editor option $1"
        exit $?
    fi
fi
