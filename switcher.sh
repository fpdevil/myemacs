#!/bin/bash
# shell script for switching between Aquamacs and Spacemacs
# the script should be run from the users home directory
#
# Aquamacs configuration file is aquamacs
#          configuration directory is aquamacs.d
# Spacemacs configuration file is spacemacs
#           configuration directory is spacemacs.d
# Custom configuration file is cmacs
#           configuration directory is cmacs.d
#
# the script is executed with an option A or S for
# switching to either Aquamacs or Spacemacs by creating
# softlinks to appropriate files and directories
#
# This script should be executed from users HOME directory
# ./switcher.sh S | ./switcher.sh A | ./switcher.sh C | ./switcher.sh B
#
# Base files and directories for each
# Aquamacs  - ~/aquamacs & ~/aquamacs.d
# Spacemacs - ~/spacemacs & ~/spacemacs.d
# Blingmacs - ~/bling & ~/bling.d
# Custommacs - ~/cmacs & ~/cmacs.d
# Smacs - ~/smacs & ~/smacs.d
############################################################

usage()
{
    echo "Usage $0 <editor code>"
    echo "provide an editor code to switch..."
    echo "@ A - aquamacs"
    echo "@ S - spacemacs"
    echo "@ C - chen"
    echo "@ B - bing"
    echo "@ X - custom"
    return 1
}

# acronyms for the emacs flavours
A=Aquamacs
S=Spacemacs
C=Chen
B=Bling
X=Custom

clear

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
                rm -rvf aquamacs.d/SessionDesktop.el aquamacs.d/custom.el
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
                echo "cleaning up leftovers..."
                rm -vfr ~/spacemacs.d/SessionDesktop.el ~/spacemacs.d/.custom.el
                exit $?
            else
                echo "spacemacs directories does not exist for $S and $1 change"
                exit $?
            fi
        fi
    # check for option Chenmacs
    elif [[ $1 == C ]]
    then
        cl=cmacs
        cld=cmacs.d
        # if [[ -L .emacs  &&  -L .emacs.d ]]
        if [[ -L .emacs  &&  -L .emacs.d ]] && [[ $(readlink -n .emacs.d) == $cld  && $(readlink -n .emacs) == $cl ]]
        then
            echo "Option $1 and Already at $C"
            exit $?
        else
            if [[ -f cmacs && -d cmacs.d ]]
            then
                echo "switching to $C for option $1"
                echo "creating soft links for chenbin..."
                # remove any existing links to aquamacs
                rm -vrf .spacemacs .emacs .emacs.d
                ln -s $cl .emacs
                ln -s $cld .emacs.d
                exit $?
            else
                echo "chenbin directories does not exist for $C and $1 change"
                exit $?
            fi
        fi
    # check for option Custom
    elif [[ $1 == X ]]
    then
        xl=scimax
        xld=scimax.d
        # if [[ -L .emacs  &&  -L .emacs.d ]]
        if [[ -L .emacs  &&  -L .emacs.d ]] && [[ $(readlink -n .emacs.d) == $xld  && $(readlink -n .emacs) == $xl ]]
        then
            echo "Option $1 and Already at $X"
            exit $?
        else
            if [[ -f scimax && -d scimax.d ]]
            then
                echo "switching to $X for option $X"
                echo "creating soft links for sampath..."
                # remove any existing links to aquamacs
                rm -vrf .spacemacs .emacs .emacs.d
                ln -s $xl .emacs
                ln -s $xld .emacs.d
                exit $?
            else
                echo "custom directories does not exist for $X and $1 change"
                exit $?
            fi
        fi
    # check for option test configuration BlingMacs
    elif [[ $1 == B ]]
    then
        ul=bling
        uld=bling.d
        # if [[ -L .emacs  &&  -L .emacs.d ]]
        if [[ -L .emacs  &&  -L .emacs.d ]] && [[ $(readlink -n .emacs.d) == $uld  && $(readlink -n .emacs) == $ul ]]
        then
            echo "Option $1 and Already at $T"
            exit $?
        else
            if [[ -f bling && -d bling.d ]]
            then
                echo "switching to $T for option $1"
                echo "creating soft links for Bling Emacs..."
                # remove any existing links to other emacs
                rm -vrf .spacemacs .emacs .emacs.d
                ln -s bling .emacs
                ln -s bling.d .emacs.d
                exit $?
            else
                echo "BlingMacs directories does not exist for $T and $1 change"
                exit $?
            fi
        fi
    else
        echo "invalid editor option $1"
        exit $?
    fi
fi
