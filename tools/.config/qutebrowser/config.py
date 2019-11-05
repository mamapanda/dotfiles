# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401

config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103

config.bind("pb", "spawn --userscript qute-bitwarden")
config.bind(";", "set-cmd-text :")
config.unbind(":")
for binding, command in c.bindings.default["normal"].items():
    if binding[0] == ";":
        config.bind(":" + binding[1:], command)

c.editor.command = ["emacs", "--no-desktop", "{file}"]

c.fonts.hints = "11pt monospace"
c.fonts.monospace = "consolas"

c.hints.chars = "hjklasdfgyuiopqwertnmzxcvb"
# c.hints.uppercase = True

c.tabs.background = True
c.tabs.last_close = "close"

c.url.default_page = "https://www.startpage.com"
c.url.searchengines = {
    "DEFAULT": "https://www.startpage.com/do/dsearch?query={}",
    "google": "https://www.google.com/search?q={}",
    "jisho": "https://jisho.org/search/{}",
    "youtube": "https://www.youtube.com/results?search_query={}",
}
c.url.start_pages = ["https://www.startpage.com"]
