import java.util.ArrayList;
import java.util.Collections;
import java.util.Arrays;
import java.util.Random;
public class SmartAISolver extends GameSolver {
    Board newboard;
    int n1,newcolor;
    int[][] BoxArray;
    int vedge[][],hedge[][];
    int u,v; //sides 3 variables
    int x,y;//sides 1 variables
    int count; // count for sacrificing
    int direction;
    boolean loop;
    public static void main(String[] args){
    
    }
    private Edge takeedge(int direction,int x1,int y1) {    //Set hedge if direction=1 and vedge if direction=2.
 if (direction>1) return new Edge(x1,y1,true);
 else return new Edge(x1,y1,false);
    }

    public Edge getNextMove(Board board, int color) {
 //initialization of board attributes
 newboard = board;
 vedge = board.vEdge;
 hedge = board.hEdge;
 newcolor = color;
 hedge = new int[board.n][board.n-1];
 vedge = new int[board.n-1][board.n];
 n1 = board.n-1;
 
 loop = false;
 count = 0;
 BoxArray = new int[n1][n1];
 for(int i = 0; i<=n1;i++){
     for(int j =0;j<n1;j++){
  hedge[i][j] = board.vEdge[i][j];
     }
 }
  for(int i = 0; i<n1;i++){
     for(int j =0;j<=n1;j++){
  vedge[i][j] = board.hEdge[i][j];
     }
 }
  for(int i = 0; i<n1;i++){
     for(int j =0;j<n1;j++){
  BoxArray[i][j] = board.getEdgeCount(i,j);
     }
 }
 //hedge[n1][n1] = 0;
 //vedge[n1][n1] = 0;
 //end of initialization
 if (takesafe3s() != null) {
   return takesafe3s();
 }
 if (sides3()) {
     if (sides01()) {
        Edge e2 = takeall3s();
        
        if ( e2 != null) {
         
          return e2;
        }
  else return takeedge(direction,x,y);
     } else {
        Edge e = sacrifice(u,v);
        if(e!=null) {
          return e;
        }
     } 
 } else if (sides01()) {
   return takeedge(direction,x,y);
 }
 else if (singleton()) {
   return takeedge(direction,x,y);
 }
 else if (doubleton()) {
   return takeedge(direction,x,y);
 }
 return makeanymove();
    }

    private Edge takesafe3s() {     //Take all singleton and doubleton 3's.
 for (int i=0;i<n1;i++) {
     for (int j=0;j<n1;j++) {
  if (BoxArray[i][j]==3) {
      if (vedge[i][j]<1) {
   if (j==0 || BoxArray[i][j-1]!=2) return setvedge(i,j);
      } else if (hedge[i][j]<1) {
   if (i==0 || BoxArray[i-1][j]!=2) return sethedge(i,j);
      } else if (vedge[i][j+1]<1) {

   if (j==n1-1 || BoxArray[i][j+1]!=2) return setvedge(i,j+1);
      } else {
   if (i==n1-1 || BoxArray[i+1][j]!=2) return sethedge(i+1,j);
      }
  }
     }
 }
 return null;
    }
    private Edge sethedge(int i, int j){
 return new Edge(i,j,false);
    }
    private Edge setvedge(int i, int j){
 return new Edge(i,j,true);
    }
    private boolean sides3() {     //Returns true and u,v if there is a BoxArray(u,v)=3.
 for (int i=0;i<n1;i++) {
     for (int j=0;j<n1;j++) {
  if (BoxArray[i][j]==3) {
      u=i;
      v=j;
      return true;
  }
     }
 }
 return false;
    }

    private Edge takeall3s() {
 if (sides3()) return takeBoxArray(u,v);
 else return null;
    }

    private boolean sides01() {     //Returns true and direction,x,y if there is a safe edge(x,y).
      Random r = new Random();
 if (r.nextInt(2)==1) direction=1; else direction=2;  //direction=1 if horizontal, direction=2 if vertical
 int i=r.nextInt(n1);
 int j=r.nextInt(n1);
 if (direction==1) {
     if (tranversehedge(i,j)) return true;
     else {
  direction=2;
  if (tranversevedge(i,j)) return true;
     }
 } else {
     if (tranversevedge(i,j)) return true;
     else {
  direction=1;
  if (tranversehedge(i,j)) return true;
     }
 }
 return false;
    }

    private boolean safehedge(int i,int j) {     //Returns true if (i,j) is a safe hedge

 if (hedge[i][j]<1) {
     if (i==0) {
  if (BoxArray[i][j]<2) return true;
       } else if (i==n1) {
  if (BoxArray[i-1][j]<2) return true;
         }
     else if (BoxArray[i][j]<2 && BoxArray[i-1][j]<2) return true;
 }
 return false;
     }

    private boolean safevedge(int i,int j) {

 if (vedge[i][j]<1) {

     if (j==0) {
  if (BoxArray[i][j]<2) return true;
       } else if (j==n1) {
  if (BoxArray[i][j-1]<2) return true;
         }
     else if (BoxArray[i][j]<2 && BoxArray[i][j-1]<2) return true;
 }
 return false;
     }

    private boolean tranversehedge(int i,int j) {
 x=i;
 y=j;
 do {
     if (safehedge(x,y)) return true;
     else {
  y++;
  if (y==n1) {
      y=0;
      x++;
      if (x>n1) x=0;
  }
     }
 }  while (x!=i || y!=j);
 return false;
     }
    private boolean tranversevedge(int i,int j) {
 x=i;
 y=j;
 do {
     if (safevedge(x,y)) return true;
     else {
  y++;
  if (y>n1) {
      y=0;
      x++;
      if (x==n1) x=0;
  }
     }
 } while (x!=i || y!=j);
 return false;
     }

    private boolean singleton() {     //Returns true and direction,x,y if edge(x,y) gives exactly
 int numb;              //1 square away
 for (int i=0;i<n1;i++) {
     for (int j=0;j<n1;j++) {
  if (BoxArray[i][j]==2) {
      numb=0;
      if (hedge[i][j]<1) {
   if (i<1 || BoxArray[i-1][j]<2) numb++;
      }
      direction=2;
      if (vedge[i][j]<1) {
   if (j<1 || BoxArray[i][j-1]<2) numb++;
   if (numb>1) {
       x=i;
       y=j;
       return true;
   }
      }
      
      if (vedge[i][j+1]<1) {
   if (j+1==n1 || BoxArray[i][j+1]<2) numb++;
   if (numb>1) {
       x=i;
       y=j+1;
       return true;
   }
      }
      direction=1;
      if (hedge[i+1][j]<1) {
   if (i+1==n1 || BoxArray[i+1][j]<2) numb++;
   if (numb>1) {
       x=i+1;
       y=j;
       return true;
   }
      }
  }
     }
 }
 return false;
    }

    private boolean doubleton() {     //Returns true and direction,x,y if edge(x,y) gives away 
 direction=2;                  //exactly 2 squares
 for (int i=0;i<n1;i++) {
     for (int j=0;j<n1-1;j++) {
  if (BoxArray[i][j]==2 && BoxArray[i][j+1]==2 && vedge[i][j+1]<1) {
      if (ldub(i,j) && rdub(i,j+1)) {
   x=i;
   y=j+1;
   return true;
      }
  }
     }
 }
 direction=1;
 for (int j=0;j<n1;j++) {
     for (int i=0;i<n1-1;i++) {
  if (BoxArray[i][j]==2 && BoxArray[i+1][j]==2 && hedge[i+1][j]<1) {
      if (udub(i,j) && ddub(i+1,j)) {
   x=i+1;
   y=j;
   return true;
      }
  }
     }
 }
 return false;
     }

    private boolean ldub(int i,int j) {      //Given BoxArray(i,j)=2 and vedge(i,j+1)=0, returns true
 if (vedge[i][j]<1) {      //if the other free edge leads to a BoxArray<2
     if (j<1 || BoxArray[i][j-1]<2) return true; 
 } else if (hedge[i][j]<1) {
     if (i<1 || BoxArray[i-1][j]<2) return true;
 } else if (i==n1-1|| BoxArray[i+1][j]<2) {
     return true;
  }
 return false;
     }

    private boolean rdub(int i,int j) {
 if (vedge[i][j+1]<1) {
     if (j+1==n1 || BoxArray[i][j+1]<2) return true;
 } else if (hedge[i][j]<1) {
     if (i<1 || BoxArray[i-1][j]<2) return true;
 } else if (i+1==n1 || BoxArray[i+1][j]<2) {
     return true;
  }
 return false;
     }

    private boolean udub(int i,int j) {
 if (hedge[i][j]<1) {
     if (i<1 || BoxArray[i-1][j]<2) return true;
 } else if (vedge[i][j]<1) {
     if (j<1 || BoxArray[i][j-1]<2) return true;
 } else if (j==n1-1 || BoxArray[i][j+1]<2) {
     return true;
  }
 return false;
     }

    private boolean ddub(int i,int j) {
 if (hedge[i+1][j]<1) {
     if (i==n1-1 || BoxArray[i+1][j]<2) return true;
 } else if (vedge[i][j]<1) {
     if (j<1 || BoxArray[i][j-1]<2) return true;
 } else if (j==n1-1 || BoxArray[i][j+1]<2) {
     return true;
  }
 return false;
     }

    private Edge sacrifice(int i,int j) {     //sacrifices two squares if there are still 3's

 incount(0,i,j);
 if (sides3not(i,j)) return takeBoxArray(u,v);
 else{
   Edge e1 = takeall3s();
     if (count+newboard.getRedScore()+newboard.getBlueScore()==n1*n1 && e1 != null) {
       
  return e1;
     } else {
  count = 0;
  incount(0,i,j);
  if (loop) {
      count=count-2;
  }
  if(outcount(0,i,j)!=null) return outcount(0,i,j);
  i=n1;
  j=n1;
      }
 }
 return null;
    }

    private void incount(int k,int i,int j) {  //enter with BoxArray[i][j]=3 and k=0
 count++;               //returns count = number in chain starting at i,j
 if (k!=1 && vedge[i][j]<1) {     //k=1,2,3,4 means skip left,up,right,down.
     if (j>0) {
  if (BoxArray[i][j-1]>2) {
      count++;
      loop=true;
  } else if (BoxArray[i][j-1]>1) incount(3,i,j-1);
     }
 } else if (k!=2 && hedge[i][j]<1) {
     if (i>0) {
  if (BoxArray[i-1][j]>2) {
      count++;
      loop=true;
   } else if (BoxArray[i-1][j]>1) incount(4,i-1,j);
     }
 } else if (k!=3 && vedge[i][j+1]<1) {
     if (j<n1-1) {
  if (BoxArray[i][j+1]>2) {
      count++;
      loop=true;
   } else if (BoxArray[i][j+1]>1) incount(1,i,j+1);
     }
 } else if (k!=4 && hedge[i+1][j]<1) {
     if (i<n1-1) {
  if (BoxArray[i+1][j]>2) {
      count++;
      loop=true;
   } else if (BoxArray[i+1][j]>1) incount(2,i+1,j);
     }
 }
    }


    public boolean sides3not(int x,int y) {
 for (int i=0;i<n1;i++) {
     for (int j=0;j<n1;j++) {
  if (BoxArray[i][j]==3) {
      if (i!=x || j!=y) {
   u=i;
   v=j;
   return true;
      }
  }
     }
 }
 return false;
     }

    private Edge takeBoxArray(int i,int j) {
 if (hedge[i][j]<1) return sethedge(i,j);
 else if (vedge[i][j]<1) return setvedge(i,j);
 else if (hedge[i+1][j]<1) return sethedge(i+1,j);
 else return setvedge(i,j+1);
    }

    private Edge outcount(int k,int i,int j) {     //Takes all but count-2 squares and exits
 if (count>0) {
     if (k!=1 && vedge[i][j]<1) {
  if (count!=2) return setvedge(i,j);
  count--;
  outcount(3,i,j-1);
      } else if (k!=2 && hedge[i][j]<1) {
  if (count!=2) return sethedge(i,j);
  count--;
  outcount(4,i-1,j);
      } else if (k!=3 && vedge[i][j+1]<1) {
  if (count!=2) return setvedge(i,j+1);
  count--;
  outcount(1,i,j+1);
      } else if (k!=4 && hedge[i+1][j]<1) {
  if (count!=2) return sethedge(i+1,j);
  count--;
  outcount(2,i+1,j);
      }
 }
  return null;
    }

    private Edge makeanymove() {
 x=-1;
 for (int i=0;i<=n1;i++) {
     for (int j=0;j<n1;j++) {
  if (hedge[i][j]<1) {
      x=i;
      y=j;
      i=n1+1;
      j=n1;
   }
     }
 }
 if (x<0) {
     for (int i=0;i<n1;i++) {
  for (int j=0;j<=n1;j++) {
      if (vedge[i][j]<1) {
   x=i;
   y=j;
   i=n1;
   j=n1+1;
      }
  }
     }
     return setvedge(x,y);
 } else {
     return sethedge(x,y);
 }
 
}
}