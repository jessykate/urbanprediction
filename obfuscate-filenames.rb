#!/usr/bin/env ruby

# rename streetview download files in a directory to random strings and output
# a CSV file with the mapping. 

# get the dir to process and normalize format of directory listing. 
process_dir = ARGV[0]
process_dir.chomp('/')

# open a csv for documenting the file rename mapping
mapping = File.new "mapping-#{Time.now}.csv", 'w'
mapping.puts("original, obfuscated")

# iterate over the files, and copy each file to a new name
# then add the mapping to the obfuscation-record. 
Dir.chdir process_dir
Dir.glob('streetview*') do |f|
	# generate a random short-ish string for the new file name 
	o =  [('a'..'z'),('A'..'Z')].map{|i| i.to_a}.flatten
	rand_string = (0..10).map{ o[rand(o.length)]  }.join

	puts "cp #{f} #{rand_string}.jpg"
	%x[cp "#{f}" #{rand_string}.jpg]
	mapping.puts("#{f}, #{rand_string}.jpg")
end

mapping.close


