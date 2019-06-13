class Term
  def inspect
    to_s
  end
end

class Atomic < Term
  attr_reader :value

  def initialize(value)
    @value = value
  end

  def to_s
    "Atom #{value}"
  end

  # Unify methods

  def unify(term, instances={})
    # Dispatch atom type
    term.unify_atom(self, instances)
  end

  def unify_atom(term, instances)
    # Atoms unify if their value is the same
    (value == term.value) ? {} : nil
  end

  def unify_variable(term, instances)
    # Delegate to the Variable class
    term.unify_atom(self, instances)
  end

  def unify_functor(term, instances)
    # Delegate to the Functor class
    term.unify_atom(self, instances)
  end

end

class Variable < Term
  attr_reader :name

  def initialize(name)
    @name = name
  end

  def to_s
    "Var #{name}"
  end

  def is_bounded?
    false
  end

  # Unify methods

  def unify(term, instances={})
    # Dispatch variable type
    term.unify_variable(self, instances)
  end

  def unify_atom(term, instances)
    # Variable unify with atom with the variable instantiated to the atom
    # if not already instantiated
    if !instances.any? { |k, _| k.name==name && k.is_bounded? }
      {BoundedVariable.new(self) => term}
    # It may happen that the instantiaton is the same
    elsif instances[self] == term
      {}
     # Otherwise unification is not possible
    else
      nil
    end
  end

  def unify_variable(term, instances)
    # Variables unify sharing their values
    ins1 = instances.any? { |k, _| k.name==name && k.is_bounded? }
    ins2 = instances.any? { |k, _| k.name==term.name && k.is_bounded? }

    # if both are instantiated to the same thing, they unify with nothing new
    if ins1 && ins2 && instances[self] == instances[term]
      {}
    # if both are instantiated to different things then they can't
    elsif ins1 && ins2
      nil
    # if is instantiated, the other is instantiated to the value of the first
    elsif ins1
      {BoundedVariable.new(term) => instances[self]}
    elsif ins2
      {BoundedVariable.new(self) => instances[term]}
    # If both variables are unbounded but happens they are the same, nothing new
    elsif name == term.name
      {}
    # If both variables are unbounded but different, link them
    else
      {self => term}
    end
  end

  def unify_functor(term, instances)
    # Delegate to Functor class
    term.unify_variable(self, instances)
  end
end

class BoundedVariable < Variable
  def initialize(var)
    @name = var.name
  end

  def is_bounded?
    true
  end
end


class Functor < Term
  attr_reader :name

  def initialize(name, args)
    @name = name
    @args = args
  end

  def args
    @args
  end

  def to_s
    list_args = args.map { |a| a.to_s }.join(', ')
    "Functor #{name}(#{list_args}))"
  end

  # Redefinition of equality to be able to compare equal functor objects
  def ==(functor2)
    name == functor2.name && args.length == functor2.args.length
  end

  # Unify methods

  def unify(term, instances={})
    # Dispatch variable type
    term.unify_functor(self,instances)
  end

  def unify_atom(term, instances)
    # Functors never unify with atoms
    nil
  end

  def unify_variable(term, instances)
    # Helpers
    unifies = @args.inject(false) { |uni, arg| uni || arg.unify(term) == {} }
    var, instance = instances.find { |k, v| k.name==term.name && k.is_bounded? }

    # If variable is instantiated but to the same functor, nothing new
    if !var.nil? && instance == self
      {}
    # If instantiated but to something different, they don't unify
    elsif !var.nil?
      nil
    # If not instantiated, but is cyclic
    elsif unifies
      nil
    # Not instantiated and not cyclic. They unify.
    else
      {BoundedVariable.new(term) => self}
    end
  end

  def unify_functor(term, instances)
    # Functors don't unify if their name or arity are different
    return nil unless name == term.name && args.length == term.args.length

    # Check that arguments unify 1 on 1
    arg_instances = {}
    args.zip(term.args).each do |a1, a2|
      result = a1.unify(a2, instances.merge(arg_instances))
      # If a pair of arguments don't unify, the whole functor don't unify
      return nil if result.nil?
      # Merge unifications for next argument
      arg_instances.merge!(result)
    end
    arg_instances
  end
end

# Mayor tests made

# Runs ok
# izq = Functor.new('f',[Variable.new('X'),Atomic.new('b')])
# der = Functor.new('f',[Functor.new('g',[Atomic.new('a')]),Atomic.new('b')])
# puts izq.unify(der) # --> {Var X=>Functor g(Atom a))}

# Runks ok
# sta = Functor.new('prex',[Variable.new('X'),Functor.new('postx',[Variable.new('X')])])
# stb = Functor.new('prex',[Functor.new('f',[Atomic.new('a')]),Functor.new('postx',[Functor.new('f',[Atomic.new('a')])])])
# sta.unify(stb) # -->  {Var X=>Functor f(Atom a))}