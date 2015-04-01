package pap.lab03;

import java.util.Arrays;
import java.util.List;
import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toList;

public class TestDishes {

	public static void main(String[] args) {
	    final List<Dish> menu =
	            Arrays.asList( new Dish("pork", false, 800, Dish.Type.MEAT),
	                           new Dish("beef", false, 700, Dish.Type.MEAT),
	                           new Dish("chicken", false, 400, Dish.Type.MEAT),
	                           new Dish("french fries", true, 530, Dish.Type.OTHER),
	                           new Dish("rice", true, 350, Dish.Type.OTHER),
	                           new Dish("season fruit", true, 120, Dish.Type.OTHER),
	                           new Dish("pizza", true, 550, Dish.Type.OTHER),
	                           new Dish("prawns", false, 400, Dish.Type.FISH),
	                           new Dish("salmon", false, 450, Dish.Type.FISH));

	    List<String> lowCaloricDishesName = 
	    		  menu.stream()
	    		      .filter(d -> d.getCalories() < 400)
	    		      .sorted(comparing(Dish::getCalories))
	    		      .map(Dish::getName)
	    		      .collect(toList());	 

	    lowCaloricDishesName.stream().forEach((x) -> {
	    	System.out.println(x);
	    });
	    
	    // lowCaloricDishesName.stream().forEach(System.out::println);
	    
	}

}
