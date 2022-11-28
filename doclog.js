window.addEventListener("load", () => {
    const searchBox = document.getElementById("search");
    const predicates = document.getElementById("predicates");

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
});
