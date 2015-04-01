package pap.test;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import static java.util.Comparator.*;

interface Fruit {
	int getWeight();
}

class Apple implements Fruit {
	private int weight;
	public Apple(int weight){
		this.weight = weight;
	}
	public int getWeight() {
		return weight;
	}	
	public String toString(){
		return "Apple:"+weight;
	}
	
}

class Orange implements Fruit {
	private int weight;
	public Orange(int weight){
		this.weight = weight;
	}	
	@Override
	public int getWeight() {
		// TODO Auto-generated method stub
		return weight+1;
	}	
	
	public String toString(){
		return "Orange:"+weight;
	}
}

public class TestLambda {

	public static void main(String[] args){

		List<Fruit> inventory = Arrays.asList(new Apple(18), new Apple(5), new Orange(7));	
		
		Comparator<Fruit> c = comparing( (Fruit f) -> f.getWeight());
		Comparator<Fruit> c1 = comparing( (Object f) -> f.toString());
		// Comparator<Fruit> c2 = Comparator.comparing( (Apple f) -> f.getWeight());
		inventory.sort(c);
		
		inventory.stream().forEach(System.out::println);
		
	}
}
