
#export ROOTDIR_PATH=$(dirname "$0")
#export SCRIPTDIR_PATH="$( cd "$( dirname "$0" )" && pwd )"
# Verify they are sourcing the file
if [[ $0 == $BASH_SOURCE ]]; then
	echo "You must source the setenv.sh file..."
	exit 1
fi
export SCRIPTDIR_PATH="$PWD"
export ROOTDIR_PATH="$SCRIPTDIR_PATH/"
export DFFPATH="$ROOTDIR_PATH/data/dff/"
export nitf_extract_path="$ROOTDIR_PATH/data/"
export nitf_extract_exe="$ROOTDIR_PATH/bin/"
echo "Extract ENV paths set ..."
