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
    "DEFAULT": "https://www.startpage.com/do/dsearch?query={}"
}
c.url.start_pages = ["https://www.startpage.com"]
