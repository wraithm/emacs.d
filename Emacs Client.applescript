tell application "iTerm"
	try
		do shell script "/usr/local/bin/emacsclient -c -n -e '(toggle-frame-maximized)'"
		tell application "Emacs" to activate
	on error
		do shell script "/Applications/Emacs.app/Contents/MacOS/Emacs --daemon"
		do shell script "/usr/local/bin/emacsclient -c -n -e '(toggle-frame-maximized)'"
	end try
end tell