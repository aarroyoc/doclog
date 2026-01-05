const DISABLE_SYNTAX_SETTING = "disableSyntaxHighlight";

window.addEventListener("load", () => {
    const searchBox = document.getElementById("search");
    const predicates = document.getElementById("predicates");
    const disableSyntax = document.getElementById("disable-syntax");

    fetch("/search-index.json")
	.then((res) => res.json())
	.then((data) => {
	    for(let predicate of data) {
		let node = document.createElement("option");
		node.value = predicate.predicate;
		predicates.appendChild(node);
	    }

	    searchBox.oninput = () => {
		for(let predicate of data) {
		    if(searchBox.value === predicate.predicate) {
			window.location.href = predicate.link;
		    }
		}
	    };
	});

    disableSyntax.onclick = () => {
        if (localStorage.getItem(DISABLE_SYNTAX_SETTING) !== "true") {
            localStorage.setItem(DISABLE_SYNTAX_SETTING, "true");
        } else {
            localStorage.removeItem(DISABLE_SYNTAX_SETTING);
        }
        location.reload();
    };

    if(localStorage.getItem(DISABLE_SYNTAX_SETTING) !== "true") {
        hljs.highlightAll();
    }
});

