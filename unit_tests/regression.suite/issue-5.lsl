integer menuChannel = 1;

showDialog(string user, string description, list buttons) {
    llDialog(user, description, buttons, menuChannel);
}

default {
    state_entry() {
        string pageString = " ";
        list options = [];
        showDialog(llGetOwner(), "Test", ["◀" + " Prev" + pageString, "▼" + " Back", "▶" + " Next" + pageString] + llList2List(options, page * 9, page * 9 + 8);
    }
}