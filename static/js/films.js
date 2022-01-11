search_input = document.getElementById("search_input");
results_container = document.getElementById("results_container");
searchs_container = document.getElementById("searchs_container");
//
bannerInt = 0;

/*Class name*/
const btn_change_banner = document.querySelectorAll('.btn_change_banner');
const banner = document.querySelectorAll('.banner');
const film_banner = document.querySelectorAll('.film_banner');
const container_film_searched = document.querySelectorAll('.container_film_searched');
const icon_rate = document.querySelectorAll('.icon_rate');

function changeBanner () {
    if(this.innerText == "❯")
        bannerInt = bannerInt + 1;
    else
        bannerInt = bannerInt - 1;
    if (bannerInt < 0)
        bannerInt = bannerInt + 6
    if (bannerInt >= 6)
        bannerInt = bannerInt - 6
    for( i = 0 ; i < banner.length ; i++  ){
        res = (bannerInt % 6) ;
        if(res == i)
            banner[i].classList.add('active');
        else
            banner[i].classList.remove('active');
    }
//    banner.forEach((item) => item.);
}
function changeBannerOver (){
    film_banner.forEach((item) =>
        item.classList.remove('active'));
    this.classList.add('active');
    for( i = 0 ; i < film_banner.length ; i++  ){
        if(film_banner[i].classList.contains('active'))
            bannerInt = i;
    }
    for( i = 0 ; i < banner.length ; i++  ){
        res = (bannerInt % 6) ;
        if(res == i)
            banner[i].classList.add('active');
        else
            banner[i].classList.remove('active');
    }
}
function checkExitSearchedId (id_s){
    ret = true;
    const container_film_searched = document.querySelectorAll('.container_film_searched');
    resizeSearch();
    for( j=0 ; j < container_film_searched.length ; j++ ){
        //const container_film_searched = document.querySelectorAll('.container_film_searched');
        id_in = container_film_searched[j].children[0].value;
        resizeSearch();
        if(id_in == id_s)
            ret = false;
        //const container_film_searched = document.querySelectorAll('.container_film_searched');
    }
    return ret;
}
function resizeSearch (){
    if(search_input.value != ""){
        if(searchs_container.classList.contains('searched')){
            results_container.style.maxHeight = results_container.scrollHeight + "px";
        }
        else
            results_container.style.maxHeight = null;
    }
    else{
        results_container.innerHtml = "";
        results_container.style.maxHeight = null;
    }

}
//var timeoutID;
function searchBy(to_search, type_s){
    if(search_input.value != ""){
        $.ajax ({ type: "GET",
                  url: "/search/" + type_s + "/" + to_search,
                  dataType: "html",
                  success: function (data) {
                      //id_film
                      var htmlObject = $(data);
                      if(search_input.value != ""){
                          for( i=0 ; i < htmlObject.length ; i++ ){
                              //console.log(htmlObject[i].children[0].value);
                              if(checkExitSearchedId (htmlObject[i].children[0].value)){
                                  if(search_input.value != "")
                                      results_container.innerHTML  = results_container.innerHTML + htmlObject[i].outerHTML;
                                  else
                                      results_container.innerHtml = "";
                              }
                          };
                      };
                      resizeSearch();
                      const container_film_searched = document.querySelectorAll('.container_film_searched');
                      container_film_searched.forEach((item) =>
                          item.addEventListener('click',addGoFilm));

                  },
                  error:   function (jqxhr) {
                      alert ("error response: " + jqxhr.responseText);
                  },
                  complete: function () {
                      resizeSearch ();
                  }
                });
    }
    else{
        results_container.innerHtml = "";
    }
    resizeSearch ();
}
function searchFilms (){
    //window.clearTimeout(timeoutID);
    //
    //timeoutID = window.setTimeout(console.log(this.value), 20000);
    to_search = this.value;
    resizeSearch ();
    if(to_search != ""){
        this.parentElement.parentElement.classList.add('searched');
        $.ajax ({ type: "GET",
                  url: "/search/Title/" + to_search,
                  dataType: "html",
                  success: function (data) {
                      //alert ("The data was: " + data);
                      //
                      //var htmlObject = $(data);
                      results_container.innerHTML  = data;
                      searchBy(to_search, "People");
                      searchBy(to_search, "Locations");
                      searchBy(to_search, "Species");
                      searchBy(to_search, "Vehicles");

                      //searchBySpecies(to_search);
                      resizeSearch();
                      const container_film_searched = document.querySelectorAll('.container_film_searched');
                      container_film_searched.forEach((item) =>
                          item.addEventListener('click',addGoFilm));
                      /*
                      if(search_input.value != "")
                          results_container.style.height = results_container.scrollHeight + "px";
                      else
                          results_container.style.height = null;*/
                      //const container_film_searched = document.querySelectorAll('.container_film_searched');
                  },
                  error:   function (jqxhr) {
                      alert ("error response: " + jqxhr.responseText);
                  },
                  complete: function () {
                      resizeSearch ();
                  }
                });
    }
    else{
        this.parentElement.parentElement.classList.remove('searched');
        results_container.style.height = null;
        results_container.innerHtml = "";
        //resizeSearch ();
    }
    resizeSearch ();

}
function addGoFilm (){
    a = document.createElement("a");
    a.href = "/frontend/film/" + this.children[0].value;
    a.click();
}
function rateThisFilm(){
    console.log(this.getAttribute("rate_num"));
    film = $(this).closest('.film')[0];
    id_film = film.querySelector('.f_film_id');
    value_S = this.getAttribute("rate_num");
    rates = this.parentElement;
    for(i = 0; i < rates.children.length ; i ++ )
        rates.children[i].classList.remove('voted');
    this.style.color = "#c36bd9";
    this.name = "star";
    this.classList.add('voted');
    $.ajax ({ type: "PUT",
              url: "/frontend/film/rate/" + id_film.value,
              data: { value : value_S
                    },
              dataType: "text",
              success: function () {
                  //getComment (element.value)
              },
              error:   function (jqxhr) {
                  alert ("error response: " + jqxhr.responseText);
              }
            });
}
function coloredStar (){
    rates = this.parentElement;
    for(i = 0; i < rates.children.length ; i ++ ){
        if(parseInt (rates.children[i].getAttribute("rate_num")) <= parseInt (this.getAttribute("rate_num"))){
            rates.children[i].style.color = "#c36bd9";
            rates.children[i].name = "star";
        }
        else{
            rates.children[i].style.color = null;
            rates.children[i].name = "star-outline";
        }
    }
}
function coloredStarVoted (){
    rates = this.parentElement;
    votedd = true;
    for(i = 0; i < rates.children.length ; i ++ ){
        if(rates.children[i].classList.contains('voted')){
            for(j = 0 ; j <= i ; j++){
                rates.children[j].style.color = "#c36bd9";
                rates.children[j].name = "star";
            }
            for(j = i ; j < rates.children.length ; j++){
                rates.children[j].style.color = null;
                rates.children[j].name = "star-outline";
            }
            votedd = false;
        }
    }
    if(votedd){
        for(i = 0; i < rates.children.length ; i ++ ){
            rates.children[i].style.color = null;
            rates.children[i].name = "star-outline";
        }
    }
}
/*Add Event Listener*/
btn_change_banner.forEach((item) =>
  item.addEventListener('click',changeBanner));
film_banner.forEach((item) =>
  item.addEventListener('mouseover',changeBannerOver));
search_input.addEventListener('input',searchFilms);
container_film_searched.forEach((item) =>
  item.addEventListener('click',addGoFilm));
icon_rate.forEach((item) =>
  item.addEventListener('click',rateThisFilm));
icon_rate.forEach((item) =>
    item.addEventListener('mouseover',coloredStar));

icon_rate.forEach((item) =>
    item.addEventListener('mouseout',coloredStarVoted));
