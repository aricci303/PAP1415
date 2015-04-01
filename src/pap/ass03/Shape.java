package pap.ass03;

/**
 * Interfaccia che rappresenta una figura
 * in una viewport grafica (0,0)-(w,h)
 * 
 * @author aricci
 *
 */
public interface Shape {
	void move(V2d v);	
	double getPerim();
	boolean isInside(P2d p1, P2d p2);
	boolean contains(P2d p);
}
