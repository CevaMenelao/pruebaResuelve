

const btn_select = document.querySelectorAll('.btn_select');
const btn_select_more = document.querySelectorAll('.btn_select_more');

function showSub(){
    btn_select.forEach((item) =>
        item.classList.remove('active'));
    this.classList.add('active');
}
function toggleActive(){
    this.classList.toggle('active')
}
btn_select.forEach((item) =>
  item.addEventListener('click',showSub));
btn_select_more.forEach((item) =>
  item.addEventListener('click',toggleActive));
