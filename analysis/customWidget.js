// Add widget for nodes
var showCustomWidget = function(props) {

  // aggregate attributes
  var attributes = ["id", "article_count", "deg_in", "deg_out", "betw", "closen", "page_rank", "eigenv", "closen", "closen_in", "closen_out", "popularity_ranking", "claim", "total_visits", "redaktionsstaerke", "referrals", "standort", "positive", "negative", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust"];

  // add them to the widget
  attributes.forEach(function(attribute) {
    var attribute_element = document.querySelector(`.${attribute} span`);
    if(!isNaN(props[attribute]) && props[attribute] % 1 != 0) {
      attribute_element.textContent = props[attribute].toFixed(6);
    } else {
      attribute_element.textContent = props[attribute];
    }
  });
}

var initializeWidget = function() {
  body = document.querySelector("body");
  body.insertAdjacentHTML("beforeend", customWidget);
}

var customWidget = `
  <div id="customWidget">

    <div class="header">
      <div class="id">
        <span>Kein Knoten ausgewählt.</span>
      </div>
    </div>

    <div class="main-content">

    <div class="website-description container">
      <div class="article_count">
        Veröffentlichte Artikel:
        <span></span>
      </div>
        <div class="popularity_ranking">
          Popularity Ranking:
          <span></span>
        </div>
        <div class="claim">
          Claim:
          <span></span>
        </div>
        <div class="total_visits">
          Totale Visits:
          <span></span>
        </div>
        <div class="referrals">
          Referrals (in %):
          <span></span>
        </div>
        <div class="redaktionsstaerke">
          Redaktionsstärke:
          <span></span>
        </div>
        <div class="standort">
          Standort (laut Impressum):
          <span></span>
        </div>
      </div>

    </div>

    <hr noshade>

    <div class="content">

      <div class="header">
        Netzwerkmaße
      </div>

      <div class="website-data container numbers">
        <div class="deg_in">
          <span></span>
          Deg In
        </div>
        <div class="deg_out">
          <span></span>
          Deg Out
        </div>
        <div class="betw">
          <span></span>
          Betweenness
        </div>
        <div class="closen">
          <span></span>
          Closeness
        </div>
        <div class="closen_in">
          <span></span>
          Closeness (Incoming Degrees)
        </div>
        <div class="closen_out">
            <span></span>
            Closeness (Outgoing Degrees)
        </div>
        <div class="eigenv">
          <span></span>
          Eigenvector-Zentralität
        </div>
        <div class="page_rank">
          <span></span>
          Page Rank
        </div>
      </div>
    </div>

    <hr noshade>

    <div class="content">
      <div class="header">
        Sentimentanalyse
      </div>
      <div class="sentiments container">
        <div class="positive">
          <span></span>
          Positive
        </div>
        <div class="negative">
          <span></span>
          Negative
        </div>
        <div class="anger">
          <span></span>
          Anger
        </div>
        <div class="anticipation">
          <span></span>
          Anticipation
        </div>
        <div class="disgust">
          <span></span>
          Disgust
        </div>
        <div class="fear">
          <span></span>
          Fear
        </div>
        <div class="joy">
          <span></span>
          Joy
        </div>
        <div class="sadness">
          <span></span>
          Sadness
        </div>
        <div class="surprise">
          <span></span>
          Surprise
        </div>
        <div class="trust">
          <span></span>
          Trust
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
      <div class="header">
        Kanten
      </div>
      <div class="edge container">
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


// add button to change sizes

var changeSizeOfNode = function(props) {

}



//////// helper source code for visNetwork file

var helperSrc = `
<script src="./../customWidget.js" type="text/javascript"></script>
<link rel="stylesheet" href="./../customWidget.css">
`
