# From: https://stackoverflow.com/questions/39464975/launch-emacsclient-with-gui-from-dock-on-mac-os-x
#
# To launch emacsclient from eg the Dock or Spotlight, it's easy to use Automator. Automator is built-in to macOS.
#
# Choose to make an "Application", then choose "Run Shell Script", and add a modified version of the above call to
# emacsclient:
#
# /usr/local/bin/emacsclient -n -c -a "" -- "$@"
#
# Then change "Pass input": use "as arguments" instead of "to stdin".
#
# The added "$@" is where any optional arguments passed to this shell script will be placed. Here, this allows you to
# pass a filename to open with emacsclient. Automator automates passing this filename in when, eg, you click to open a
# file with the application we've just made. This also allows you to set the application to be the default application
# for certain file types.

/usr/local/bin/emacsclient -n -c -a "" -- "$@"
