#!/usr/bin/env ruby

require 'set'

Folder = Struct.new(:name, :contents)

# returns an array of pairs of subdirectories and their contents
def findFiles(folder)
  structure = `ls -R #{folder}`
  #separates subdirectories
  subFoldersStructure = structure.split("\n\n")
  subfLen = subFoldersStructure.length
  subFolders = Array.new(subfLen)
  #separates subdirectories names and contents
  for i in 0...(subfLen)
    sep = subFoldersStructure[i].split("\n")
    name = sep[0].tr(':', '')
    files = sep[1..sep.length]
    #store name/contents pair
    subFolders[i] = Folder.new(name, files)
  end
  return subFolders
end

#clear existing results, if any
def clear(dirsToParse)
  dirList = `ls`
  dirsToParse.each do |dir|
    #check for previous results
    if dirList.include? "results_#{dir}"
      `rm -r results_#{dir}/`
      puts "clearing existing #{dir} results..."
    end
  end
end 

####### start script #######

dirList = `ls`
dirsToParse= Set.new()
cFlag = false; vFlag = false; hFlag = false


#check flags
for arg in ARGV
  case arg
  #verbosity
  when '-v'
    vFlag = true
  #clear
  when '-c'
    cFlag = true 
  #help
  when '-h'
    hFlag = true
  else
    dirsToParse.add(arg)
  end
end

#check for directory existence
dirsToParse.each do |dir|
  if not dirList.include? dir
    puts "cannot find local directory #{dir}"
    exit
  end
end

#clear old results
clear(dirsToParse)
if cFlag
  exit
end

#puts "got to test exit"
#exit
noOfSemanticErrs = 0
noOfSyntaxErrs = 0
noOfMiscErrs = 0
#parse directories
dirsToParse.each do |dir|

  #make results directory and find files to parse
  if vFlag
    puts "making folder #{dir}..."
  end
  `mkdir results_#{dir}`
  subfs = findFiles(dir)

  #parse subdirectories


  for f in subfs
    if vFlag
      puts "parsing folder #{f.name} ..."
    end
    #make results directory
    if not dirsToParse.include? f.name
      `mkdir results_#{f.name}`
    end
    #parse files
    for file in f.contents
      if file.include? ".wacc"
        #parse file
        resultfilename = "results_#{f.name}/result_" + file.sub(".wacc",".txt")
        `./compile #{f.name}/#{file} >> #{resultfilename}`
        #check for error
        exitCd = $?.exitstatus
        errorFind = `grep error #{resultfilename}`
        if vFlag
          isSemantic = resultfilename.include? "semantic"
          isSyntax = resultfilename.include? "syntax"
          if (not isSemantic) && (not isSyntax) && (not exitCd == 0)
            puts "error parsing VALID file #{file}, exit code #{exitCd}, expecting 0"
            puts `cat #{resultfilename} + "\n"`
            noOfMiscErrs += 1
          elsif isSemantic && (not exitCd == 200)
            puts "error parsing file #{file}, exit code #{exitCd}, expecting 200"
            noOfSemanticErrs += 1
          elsif isSyntax && (not exitCd == 100)
            puts "error parsing file #{file}, exit code #{exitCd}, expecting 100"
            noOfSyntaxErrs += 1
          elsif (not isSyntax) && (not isSemantic) && (not errorFind == '')
            puts "error parsing file #{file}"
						puts `cat #{resultfilename}` + "\n"
            noOfMiscErrs += 1
          end
        end
      end
    end
  end
end
puts "No of missed semantic errs: #{noOfSemanticErrs}"
puts "No of missed syntax errs: #{noOfSyntaxErrs}"
puts "No of unexpected errs: #{noOfMiscErrs}"


##############
