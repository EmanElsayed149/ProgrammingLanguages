#introduction to ruby
class Hello
  def print_hello_World
    puts "hello world!"
  end
end
x=Hello.new
x.print_hello_World

#Classes and Objects
class A
      def m1
        4
      end
  def m3 x
    x*m1
  end
end

#Objects States
class State
  def initialize(f=0)
    @foo =f
  end
  def m1
    @foo =0
  end
  def m2
    @foo += 17
  end
  def foo
    @foo
  end
end

class B
  Dan_age=38
  def self.reset_bar
   @@bar=0
  end
  def initialize(f=0)
    @foo=f
  end
  def m2 x
    @foo +=x
    @@bar +=1
  end
  def foo
    @foo
  end
  def bar
    @@bar
  end
end

#long example
class Myrational
  def initialize (num,den)
    if den ==0
      raise "Myrational received an inappropriate argument"
    elsif den < 0
       @num = - num
       @den = - den
     else
       @num=num
       @den =den
    end
    reduce
    end
    def to_s
     ans = @num.to_s
      if @den !=1
        ans += "/"
        ans += @den.to_s
      end
      ans
    end
    def to_s2
      dens=""
      dens += "/" + @den.to_s if @den !=1
      @num.to_s + dens
    end
    def to_s3
      "#{@num}#{if @den==1 then "" else "/" + @den.to_s end}"
    end
    def add! r
      a=r.num
      b=r.den
      c=@num
      d=@den
      @num=(a*d)+(c*b)
      @den=b*d
      reduce
     # self
    end
    # a functional addition, so we can write r1.+ r2 to make a new rational
    # and built-in syntactic sugar will work: can write r1 + r2
    def + r
      ans = Myrational.new(@num,@den)
      ans.add! r
    end
    protected
    def num
      @num
    end
    def den
      @den
    end
    private
    def gcd(x,y)
      if x==y
        x
      elsif x< y
        gcd(x,y-x)
      else
        gcd(y,x)
      end
    end
    def reduce
      if @num==0
        @def=1
      else
        d=gcd(@num.abs,@den)
        @num = @num /d
        @den = @den / d
      end
      to_s
    end
end

def use_rationals
  r1 = Myrational.new(3,4)   # r1 (3,4)
  r2 = r1 + r1 + Myrational.new(-5,2) # r2 =(
  puts r2.to_s
  (r2.add! r1).add! (Myrational.new(1,-4))
  puts r2.to_s
  puts r2.to_s2
  puts r2.to_s3
end
# blocks
def test
  @sum = 0
  [4,6,8].each {|x|@sum +=x}
  puts @sum 
end
# using blocks
def count i
  if yield i
    1
  else
    1 +( count(i +1) {|x| yield x})
  end
end

#proc class
def test_proc
  a=Array.new(5) {|i| -i}
  c=a.map {|x| lambda{|y| x>=y }}
  puts c[2].call -3
end


                                  

                    

