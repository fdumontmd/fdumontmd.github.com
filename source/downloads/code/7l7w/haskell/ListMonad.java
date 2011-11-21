package test.monad;

import java.util.ArrayList;
import java.util.List;

/* Java type system is not rich enough to be used here.
 * Even with generics, some expression do not typecheck,
 * so I use Object everywhere
 */
@SuppressWarnings({ "rawtypes", "unchecked" })
public class ListMonad {
	private List content;
	private ListMonad(final List content) {
		this.content = new ArrayList(content);
	}
	
	ListMonad bind(Func func) {
		ArrayList res = new ArrayList();
		
		for (Object obj: content) {
			res.addAll(func.run(obj).getContent());
		}
		
		return new ListMonad(res);
	}
	
	static ListMonad wrap(Object obj) {
		ArrayList res = new ArrayList();
		res.add(obj);
		
		return new ListMonad(res);
	}
	
	// error param is not used in ListMonad
	static ListMonad fail(String error) {
		return new ListMonad(new ArrayList());
	}
	
	public List getContent() {
		return content;
	}

	interface Func {
		ListMonad run(Object obj);
	}	
	
	static ListMonad monadDo(List lst) {
		return new ListMonad(lst);
	}
	
	public static void main(String [] args) {
		List output = monadDo(Sequence.makeRange(1, 5)).bind(new MakePair1()).getContent();
		
		for (Object obj: output) {
			System.out.println(obj);
		}
	}
}

class MakePair1 implements ListMonad.Func {
	public ListMonad run(Object obj) {
		ListMonad.Func func = new MakePair2(obj);
		return ListMonad.monadDo(Sequence.makeRange(1, 5)).bind(func);
	}
}

class MakePair2 implements ListMonad.Func {
	private Object content;
	public MakePair2(final Object obj) {
		this.content = obj;
	}
	
	public ListMonad run(Object obj) {
		Integer cont = (Integer) content;
		Integer val = (Integer) obj;
		if (cont.intValue() < val.intValue())
			return ListMonad.wrap(new Pair(content, obj));
		else
			return ListMonad.fail("Not a valid pair");
	}
}

@SuppressWarnings({ "rawtypes", "unchecked" })
class Sequence {
	static List makeRange(int from, int to) {
		ArrayList lst = new ArrayList();
		
		for (int i = from; i <= to; i++) lst.add(i);
		
		return lst;
	}
}

class Pair {
	private Object fst;
	private Object snd;
	
	public Pair(final Object fst, final Object snd) {
		this.fst = fst;
		this.snd = snd;
	}
	
	public String toString() {
		return "(" + getFst() + ", " + getSnd() + ")";
	}

	public Object getFst() {
		return fst;
	}

	public Object getSnd() {
		return snd;
	}
}