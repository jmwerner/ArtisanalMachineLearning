
.prepend_class = function(input, class_to_add){
    class(input) = c(class_to_add, class(input))
    input
}
