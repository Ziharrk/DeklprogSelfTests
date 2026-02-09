import java.util.*;
import java.util.function.*;

interface Numeric<T> {
  T zero();
  T add(T a, T b);

  public static Numeric<Integer> integer() {
    return new Numeric<>() {
      public Integer zero() { return 0; }
      public Integer add(Integer a, Integer b) { return a + b; }
    };
  }
}

interface Foldable<T> {
  <R> R foldr(R initial, BiFunction<T, R, R> function);

  default List<T> toList() {
    return foldr(new LinkedList<>(), (value, list) -> {
      list.add(value);
      return list;
    });
  }

  default int length() { return foldr(0, (_, result) -> result + 1); }

  default boolean contains(T value) {
    return foldr(false, (other, result) -> result || other.equals(value));
  }

  default T sum(Numeric<T> numeric) {
    return foldr(numeric.zero(), numeric::add);
  }
}

sealed interface Tree<T> extends Foldable<T>
  permits Empty, Node {}

record Empty<T>() implements Tree<T> {
  @Override
  public <R> R foldr(R initial, BiFunction<T, R, R> function) {
    return initial;
  }
}

record Node<T>(Tree<T> left, T value, Tree<T> right) implements Tree<T> {
  @Override
  public <R> R foldr(R initial, BiFunction<T, R, R> function) {
    R x = right.foldr(initial, function);
    R y = function.apply(value, x);
    return left.foldr(y, function);
  }
}

public class Main {
  public static void main(String[] args) {
    Tree<Integer> tree = new Node<>(
      new Empty<>(),
      3,
      new Node<>(new Node<>(new Empty<>(), 7, new Empty<>()), 4, new Empty<>())
    );

    System.out.println(tree.sum(Numeric.integer()));  // 14
    System.out.println(tree.toList());  // [4, 7, 3]
    System.out.println(tree.length());  // 3
    System.out.println(tree.contains(3));  // true
    System.out.println(tree.contains(9));  // false
  }
}

