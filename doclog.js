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

window.addEventListener("load", () => {
  const nav = document.getElementById("navigation");
  if (!nav) return;

  const base = document.body.getAttribute("data-base-url") || "/";
  fetch(base + "nav_menu.html")
    .then(r => r.text())
    .then(html => {
      nav.innerHTML = html;
    })
    .catch(err => console.error("Failed to load navigation:", err));
});

window.addEventListener("load", () => {
  const footer = document.getElementById("site-footer");
  if (!footer) return;

  const base = document.body.getAttribute("data-base-url") || "/";
  fetch(base + "footer.html")
    .then(r => r.text())
    .then(html => {
      footer.innerHTML = html;
    })
    .catch(err => console.error("Failed to load footer:", err));
});
