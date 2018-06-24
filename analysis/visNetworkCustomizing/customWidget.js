// Add widget for nodes
var showCustomWidget = function(props) {

  var id = document.querySelector(".name span");
  id.textContent = props.id;

  var article_count = document.querySelector(".article_count span");
  article_count.textContent = props.article_count;

  var deg_in = document.querySelector(".deg_in span");
  deg_in.textContent = props.deg_in;

  var deg_out = document.querySelector(".deg_out span");
  deg_out.textContent = props.deg_out;

  var betw = document.querySelector(".betw span");
  betw.textContent = props.betw.toFixed(6);

  var page_rank = document.querySelector(".page_rank span");
  page_rank.textContent = props.page_rank.toFixed(6);

  var eigenv = document.querySelector(".eigenv span");
  eigenv.textContent = props.eigenv.toFixed(6);

  var closen = document.querySelector(".closen span");
  closen.textContent = props.closen.toFixed(6);

  var closen_in = document.querySelector(".closen_in span");
  closen_in.textContent = props.closen_in.toFixed(6);

  var closen_out = document.querySelector(".closen_out span");
  closen_out.textContent = props.closen_out.toFixed(6);

}

var initializeWidget = function() {
  body = document.querySelector("body");
  body.insertAdjacentHTML("beforeend", customWidget);
}

var customWidget = `
  <div id="customWidget">
    <div class="content">

      <div class="website-description">
        <div class="name">
          Name:
          <span>Kein Knoten ausgewählt.</span>
        </div>
        <div class="article_count">
          Veröffentlichte Artikel:
          <span></span>
        </div>
      </div>

      <div class="website-data">
        <div class="deg_in">
          Deg In:
          <span></span>
        </div>
        <div class="deg_out">
          Deg Out:
          <span></span>
        </div>
        <div class="betw">
          Betweenness:
          <span></span>
        </div>
        <div class="closen">
          Closeness:
          <span></span>
        </div>
        <div class="closen_in">
          Closeness (Incoming Degrees):
          <span></span>
        </div>
        <div class="closen_out">
            Closeness (Outgoing Degrees):
            <span></span>
        </div>
        <div class="eigenv">
          Eigenvector-Zentralität:
          <span></span>
        </div>
        <div class="page_rank">
          Page Rank:
          <span></span>
        </div>
      </div>

    </div>
  </div>`

document.addEventListener("DOMContentLoaded", function() {
  initializeWidget();
});


// Add widget for the edges
var showCustomEdgeWidget = function(props) {
  var linked_count = document.querySelector(".linked_count span");
  linked_count.textContent = props.linked_count;
}

var initializeEdgeWidget = function() {
  body = document.querySelector("body");
  body.insertAdjacentHTML("beforeend", customEdgeWidget);
}

var customEdgeWidget = `
  <div id="customWidget">
    <div class="content">
      <div class="edge">
        <div class="linked_count">
          Anzahl der Verlinkungen:
          <span></span>
        </div>
      </div>
    </div>
  </div>`

document.addEventListener("DOMContentLoaded", function() {
  initializeEdgeWidget();
});
