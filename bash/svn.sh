###   Copyright (C) 2004-2008 Todd V. Jonker.  All Rights Reserved.
###
###   Licensed under the Apache License, Version 2.0 (the "License");
###   you may not use this file except in compliance with the License.
###   You may obtain a copy of the License at
###
###       http://www.apache.org/licenses/LICENSE-2.0
###
###   Unless required by applicable law or agreed to in writing, software
###   distributed under the License is distributed on an "AS IS" BASIS,
###   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
###   See the License for the specific language governing permissions and
###   limitations under the License.


##
##  Aliases to manipulate common SVN properties.
##

alias svn-eol-native='svn propset svn:eol-style native'
alias svn-eol-dos='svn propset svn:eol-style CRLF'
alias svn-eol-unix='svn propset svn:eol-style LF'
alias svn-eol-show='svn propget svn:eol-style'

alias svn-exe-show='svn propget svn:executable'
alias svn-exe-set='svn propset svn:executable \*'
alias svn-exe-unset='svn propdel svn:executable'


alias svn-diff='svn diff --diff-cmd /usr/bin/diff -x '\''--side-by-side --left-column --ignore-all-space --width=159'\'''


#alias svn-tunnel-cc='ssh -L 3690:svn.consciouscode.com:3690 todd@svn.consciouscode.com'



##
##  Display the SVN URL of the current directory or a given file.
##

function svn-url
{
   svn info $1 | head -2 | tail -1 | cut -f 2- -d " "
}



##
##  Tag the SVN project at the current directory with a given string.
##

function svn-tag
{
    local tag=$1
    if [ -z "$tag" ]
    then
	echo "Usage: svn-tag TAG"
	echo "  Tags a Subversion project."
	echo "  The working directory must be a working copy of a trunk."
	return 1
    fi

    local trunkUrl=$(svn-url)
    if [ ! "$(basename $trunkUrl)" = "trunk" ]
    then
	echo "You must invoke svn-tag from a Subversion trunk working copy."
	return 1
    fi

    local projectUrl=$(dirname $trunkUrl)
    local project=$(basename $projectUrl)

    local tagsUrl=$projectUrl/tags
    local destUrl=$tagsUrl/$tag

    local exist=$(svn ls $tagsUrl | grep "^$tag/$")
    if [ ! -z "$exist" ]
    then
	echo "Tag already exists: $destUrl"
	return
    fi
    
    echo "Tagging $project at $destUrl"
    svn copy $trunkUrl $destUrl -m "Tagging $project/tags/$tag from trunk."
}



##
##  Remove a tag from the SVN project at the current directory.
##

function svn-untag
{
    local tag=$1
    if [ -z "$tag" ]
    then
	echo "Usage: svn-untag TAG"
	echo "  Removes a Subversion project tag."
	echo "  The working directory must be a working copy of a trunk."
	return 1
    fi

    local trunkUrl=$(svn-url)
    if [ ! "$(basename $trunkUrl)" = "trunk" ]
    then
	echo "You must invoke svn-untag from a Subversion trunk working copy."
	return 1
    fi

    local projectUrl=$(dirname $trunkUrl)
    local project=$(basename $projectUrl)

    local tagsUrl=$projectUrl/tags
    local destUrl=$tagsUrl/$tag

    local exist=$(svn ls $tagsUrl | grep "^$tag/$")
    if [ -z "$exist" ]
    then
	echo "Tag does not exist: $destUrl"
	return
    fi
    
    echo "Removing $destUrl"
    svn rm "$destUrl" -m "Removing $project/tags/$tag"
}


function svn-new-project
{
    local projectUrl=$1
    if [ -z "$projectUrl" ]
    then
	echo "Usage: svn-new-project PROJECT_URL"
	echo "  Creates a new Subversion project."
	echo "  PROJECT_URL must identify a non-existent directory in a Subversion repository."
	echo "  The directory will be added and committed along with empty directories"
	echo "  for branches/tags/trunk.  The new trunk is then checked out into the current"
	echo "  directory."
	return 1
    fi

    local project=$(basename $projectUrl)
    if [ -e "$project" ]
    then
	echo "A file '$project' exists here, so we can't create it."
	return 1
    fi

    local repoUrl=$(dirname $projectUrl)
    if [ "." = "$repoUrl" ]
    then
	echo "Bad repository path: $projectUrl"
	return 1
    fi
    
    if ! svn ls $repoUrl >& /dev/null
    then
	echo "Parent directory doesn't exist in repository: $repoUrl"
	return 1
    fi
    
    if svn ls $projectUrl >& /dev/null
    then
	echo "Project directory already exists: $projectUrl"
	return 1
    fi

    echo "Creating new directory in repository..."
    svn mkdir -m "Creating project $project" $projectUrl || return 1

    svn co $projectUrl \
        && svn mkdir $project/branches $project/tags $project/trunk \
	|| return 1

    cat > $project/README <<EOF

This is the top of the $project repository.

   trunk/ ......... The latest development sources.  When people say
                    "Get the head of trunk", they mean the latest
                    revision of this directory tree.

   branches/ ...... Various development branches.  Typically a branch
                    contains a complete copy of trunk/, even if the
                    changes are isolated to one subdirectory.  Note
                    that branch copies are generally removed after
                    they've been merged back into trunk, so what's in
                    branches/ now does not reflect all the branches
                    that have ever been created.

   tags/ .......... Snapshots of releases.  As a general policy, we
                    don't change these after they're created; if
                    something needs to change, we move it to
                    branches and work on it there.
EOF
   
    svn add $project/README \
	&& echo "Committing new project structure" \
	&& svn ci -m "Created project branches/tags/trunk" $project \
	&& /bin/rm -rf $project \
	&& svn co "$projectUrl/trunk" $project \
	&& echo "Your new project's trunk is now here: $project"
}
