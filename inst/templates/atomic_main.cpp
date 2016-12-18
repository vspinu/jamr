
{{{include_jamr}}}

int main() {

  OTYPE vec;
  std::ifstream ifile("{{{archive}}}", std::ios::binary);
  cereal::BinaryInputArchive ibin(ifile);
  ibin(vec);

  cout << "Successfully read a vector of length " << vec.size() << endl;

  return 0;
}
