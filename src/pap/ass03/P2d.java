package pap.ass03;

/**
 * Punto in una viewport grafica 
 * 
 * @author aricci
 *
 */
public class P2d {

    private final int x,y;

    public P2d(int x,int y){
        this.x=x;
        this.y=y;
    }

    public int getX(){
    	return x;
    }
    
    public int getY(){
    	return y;
    }

    public P2d sum(V2d v){
        return new P2d(x+v.getX(),y+v.getY());
    }

    public static double distance(P2d p0, P2d p1){
    	return new V2d(p0,p1).module();
    }
    
    public String toString(){
        return "("+x+","+y+")";
    }

}
