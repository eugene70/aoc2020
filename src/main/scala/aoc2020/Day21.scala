package aoc2020

object Day21:

  case class Food(ingredients: Set[String], allergens: Set[String])
  object Food:
    def of(line: String): Food =
      line match
        case s"${ins} (contains ${als})" => Food.of(ins, als)
    def of(ins: String, als: String): Food =
      Food(ins.split(" ").toSet, als.split(", ").toSet)

  type InputType = Seq[Food]
  type Allergen = String
  type Ingredient = String

  @main def runDay21: Unit =
    val testData = time("testReady", () => ready(testInput))
    time("testPart1", () => part1(testData)) // should be 5
    time("testPart2", () => part2(testData)) // should be "mxmxvkd,sqjhc,fvjkl"

    val data = time("ready", () => ready(input))
    time("part1", () => part1(data)) // 1815
    time("part2", () => part2(data)) //

  def ready(input: String): InputType =
    input
      .linesIterator
      .map(Food.of)
      .toList

  def part1(data: InputType): Long =
    val allergenFoodMap: Map[Allergen, Seq[Set[Ingredient]]] =
      data.flatMap(f => (f.allergens.map(al => (al, f.ingredients))))
        .groupMap(_._1)(_._2)
    val allergenSet = allergenFoodMap
        .map((al, ins) => (al, ins.reduce((a, b) => a.intersect(b))))
        .flatMap((al, ins) => ins)
        .toSet
    data.flatMap(f => f.ingredients)
      .filter(!allergenSet.contains(_))
      .size

  def part2(data: InputType): String =
    val allergenFoodMap: Map[Allergen, Seq[Set[Ingredient]]] =
      data.flatMap(f => (f.allergens.map(al => (al, f.ingredients))))
        .groupMap(_._1)(_._2)
    val x = allergenFoodMap
      .map((al, ins) => (al, ins.reduce((a, b) => a.intersect(b))))
      .toList
      .sorted(Ordering.by(_._2.size))
      .map(x => {println(x); x})
      .foldLeft((Set.empty[String], Seq.empty[(String, Set[String])]))
               ((r, a) => (r._1 ++ (a._2 -- r._1), r._2 :+ (a._1, (a._2 -- r._1))))
    x._2.sorted(Ordering.by(_._1))
      .map(x => {println(x); x})
      .flatMap(_._2)
      .mkString(",")
    //println(x)
//    allergentSet
//      .toSeq
//      .sorted
//      .mkString(",")

  def extractAllergens(allergenMap: Map[Allergen, Set[Ingredient]]): Seq[(Allergen, Ingredient)] =
    ???

  val testInput =
    """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
      |trh fvjkl sbzzf mxmxvkd (contains dairy)
      |sqjhc fvjkl (contains soy)
      |sqjhc mxmxvkd sbzzf (contains fish)
      |""".stripMargin

  val input =
    """lfqhssm pcxzx jrs rxhv mhtc dvc jvd ttvtr srvzpc thzbt jfkhrp zxstb tkpqpb lcplblp jflxtnp jnqgtrq zdfl kdxht dfxfqs qdths cnzz drcj stjr jtrrnz rlh ljfsmv ptrj rdnsghf kllgt dgtxv zpxvch rfnzl xszpg qmbn nfff cxzrtx fvmzmrd szsdbv mcmrzs hfdxb slgnk szsmh zngdbgf hgndp pxxjmds pdbcdp zczv hvkvz mhzbkt qjhmth gmstmh tvmpdqp ztbgdkd xdfjsm hbfnkq fxzgqn ljclf mpzq mxsbp cjdcn jgjx gnbxs sqrk tlqtz mdghb npvv nkrpv thvbm dfdr srgh xkvgj fmj smxk khqvlxn nkvdvs tnd fqgl dhlmfn rzvj ljvx (contains dairy, sesame, nuts)
      |tfshb zxstb thzbt dfxfqs prkc qxst nzdmcf mxsbp cjdcn hfdxb hbfnkq dfdr fdknsl nvrndt hhrdrl mcmrzs nkrpv szsdbv jgkvfl slfpcp dvc nfff bmttzd snlhkm nkvdvs sdts qftpx thd qxc zpxvch rdnsghf zpp jrnqx cqvqlp tvmpdqp lbzg kcvzks grzdp zhdtt mdghb zdfl qmbn kllgt mhtc gnbxs mk fmj jmc smxk sqrk fbkpkv mhzbkt nltxdhr hjnx thtsvz fstx ffmq dvffd jfkhrp mdlr rgpvhb (contains nuts, eggs, dairy)
      |txvg zjlxft fstx mk rkrqrk flrn cqvqlp ptrj pxxjmds tlst jxj fmkmrk xxgd lfrt mhtc drcj njpx fqgl kllgt hfdxb nltxdhr ztbgdkd cnzz qxc hjnx pcxzx ljclf gqsnd svbvqlq ljvx jfkhrp ndqgjs jrs cgrjf thd jrnqx slfpcp tcxz bfrj bdclf jtnfbh zxstb rlh lbzg jvd thvbm nvrndt jtrrnz cxzrtx khqvlxn vnrnn slsdck mhzbkt jlfmg zdfl gnbxs bmxlldv sstvx (contains dairy, soy)
      |pxxjmds qdths mjpqlvj szsdbv rknm tnd sdts fstx sstvx zxstb lgfmt pdbcdp thtsvz vxmvk mdghb jrs rfnzl smxk rrlszk jgkvfl ljclf gmstmh lfrt fxzgqn hbfnkq tlqtz crrgj rtpgqd jmc rxkt zpp qnvml lgcrd zpxvch ltnqh fdrgkrx gqsnd grzdp ljvx zczv mhtc jnqgtrq srgh qlcvd cgrjf jrnqx hfdxb mxsbp srvzpc kpn lbzg qjhmth njpx crbbk jlfmg bmxlldv hkchvb dxbmdj zpvkl dvffd kllgt srj sqrk jvjtrxt ndqgjs rxhv jflxtnp jvd ffmq bghtk bhrv cxzrtx fnfm zvmnd xbjqr kkdzxr bdsxbd mxprt cqvqlp pbqn cmfjr (contains wheat, nuts, sesame)
      |hgndp qnvml mhtc krgmpn zpp fdknsl gqsnd xhxrg tlqtz hvkvz ltnqh tkpqpb bmxlldv srvzpc slgstf fvmzmrd ttvtr jtrrnz dgtxv jrnqx zhdtt lgfmt kclh xxgd jnqgtrq hbfnkq kllgt slfpcp xszpg hfdxb ljclf rxhv bfrj zxstb ztbgdkd jtnfbh hqqzdc dfdr mtlcq lgcrd xkvgj ndqgjs rxkt lfqhssm dctgv ljvx zczv szsdbv tfshb bghtk zjlxft (contains fish, soy)
      |mhtc zpp qdths slsdck rxhv fmj hbfnkq cqvqlp kpn jlfmg nnxxsjh rlh flrn slfpcp ljclf gmstmh srgh jtrrnz tlst lvtrmn hhrdrl sstvx nkvdvs dgtxv zczv vmfhv jrnqx fnfm bdclf slgstf tlqtz cgrjf stjr krgmpn tkpqpb rrlszk bmpt ljvx dvffd tnd gqsnd dfdr ztbgdkd npvv svbvqlq rgpvhb qpktd sdts fqgl qmbn gvtvmn kdxht hfdxb rxkt kcvzks kllgt rzvj slgnk qxst khqvlxn qxc rtpgqd szsmh qnhvn tzxmtx fdrgkrx vxmvk kkmvg gnbxs (contains wheat, dairy)
      |lfqhssm hhrdrl tcxz srvzpc hbfnkq jrnqx kcvzks sxtfvrhl zhdtt dvc jvd rkhs rkrqrk fbkpkv vmfhv rltxnqf rgpvhb bhrv slsdck dhhf sqrk mxsbp dvffd slsqp tlqtz nltxdhr tvmpdqp cxzrtx crbbk jfkhrp zxstb mk stjr fqll mhtc dctgv xhxrg rdnsghf hfdxb xqnrcg xbjqr slgstf qmbn qxst ztr kllgt hvkvz fdknsl xszpg npvv nzdmcf szsdbv zgcncf qpktd bdsxbd zpvkl xdfjsm ncgn ttvtr jrs gqsnd pdbcdp hgndp bmxlldv zmdl ljvx rfnzl txvg qjhmth (contains dairy, wheat, nuts)
      |hbfnkq kpn mk hfdxb szsmh kkdzxr rfnzl jflxtnp lgfmt sstvx qxst dhhf npvv zngdbgf nnxxsjh mjpqlvj jfkhrp ljvx krgmpn ztbgdkd smxk jtrrnz bdclf vhchv zpvkl gnbxs nvrndt zbkjjkz qjhmth jmc pdbcdp fmj rkrqrk mpzq kllgt gfgmz crrgj dfdr jrnqx dnjt lcplblp nfff jgkvfl fnfm dctgv mcmrzs slsdck mxprt szsdbv dfxfqs mhtc hvkvz (contains wheat)
      |bghtk hkchvb nkrpv rlh jrnqx zvmnd hfdxb hbfnkq qxst mdghb jfkhrp jlfmg lbzg tnd ljvx krgmpn ptrj xdfjsm tlqtz slsdck dgtxv fdrgkrx xkvgj qdths szsdbv kkmvg mhtc nvrndt fvmzmrd mhzbkt jnqgtrq pbqn nnxxsjh zxstb ljfsmv nzdmcf cnzz fmkmrk dfdr jtnfbh kllgt ttvtr (contains nuts, fish, soy)
      |lgcrd ljvx mhzbkt zpp bdsxbd hjnx jnqgtrq fvmzmrd jtrrnz rxhv zvmnd xszpg mhtc zbkjjkz hbfnkq lvtrmn qdths gnbxs flrn dctgv txvg hfdxb qjhmth sdts zgcncf nltxdhr tcxz jlfmg jrnqx lcplblp fmj cgrjf mpzq gvtvmn jflxtnp jgjx qlcvd mjpqlvj kllgt cnzz jmc zjlxft jgkvfl slgstf ttvtr fqll pdbcdp srgh vxmvk sxtfvrhl rknm rdnsghf bmpt kcvzks szsmh srvzpc zmdl rltxnqf dhlmfn (contains dairy, peanuts, soy)
      |zpvkl fdknsl xkvgj fvmzmrd lfqhssm txvg bdclf hbfnkq jgkvfl bmxlldv qxc dhhf xxgd gvtvmn hvkvz srgh dhlmfn zjlxft svbvqlq jtrrnz rgpvhb jtnfbh mhtc nvrndt sdts bghtk gnbxs kkmvg gfgmz dgtxv mxprt fdrgkrx jmc jvjtrxt fstx sqrk hgndp mhzbkt jflxtnp szsmh zvmnd gqsnd crrgj tnd vnrnn srj vmfhv rfnzl hqqzdc slgnk szsdbv thzbt slgstf ljvx kllgt nltxdhr zgcncf jrs kpn fxzgqn dnjt jnqgtrq jrnqx zpxvch slfpcp ltnqh zxstb dctgv sxtfvrhl qdths (contains wheat, nuts, peanuts)
      |tfshb cxzrtx tvmpdqp sqrk qlcvd rltxnqf gnbxs bdsxbd lbzg fmkmrk ffmq dhlmfn jrnqx crrgj ljclf lfrt stjr jvd hbfnkq hhrdrl dvc srvzpc flrn bfrj kllgt fdrgkrx zpvkl sdts ljvx qnvml npvv nltxdhr pbqn vhchv zmdl nfff pxxjmds xdfjsm jrs hkchvb svbvqlq hfdxb rlh jxj vmfhv zxstb kkmvg bghtk mxprt (contains fish, nuts, peanuts)
      |zdfl fqll ljfsmv dfdr ljvx slfpcp jgjx mxsbp mpzq smxk stjr hjnx jmc kcvzks slgstf bfrj bdclf hqqzdc zczv mk rdnsghf jnqgtrq ltnqh cmfjr xqnrcg szsdbv hhrdrl dgtxv gnbxs pcxzx ptrj zjlxft dvc tzxmtx mjpqlvj fvmzmrd bmpt ztbgdkd tlqtz tlst kllgt zmdl gfgmz sxtfvrhl hfdxb bmxlldv vnrnn ncgn kkmvg nltxdhr ztr hbfnkq drcj dhlmfn cqvqlp rknm vmfhv nfff npvv mdlr jrnqx cjdcn sqrk krgmpn mxprt gmstmh xxgd mhtc nvrndt fmkmrk zhdtt rkrqrk (contains eggs, sesame)
      |cmfjr qlcvd bhrv zjlxft cnzz crbbk qnvml dfdr zxstb ljvx nkvdvs szsmh tkpqpb kllgt kcvzks fqgl tlst hvkvz srj fvmzmrd lcplblp qnhvn jlfmg qftpx slgstf tfshb drcj xxgd kkdzxr rxhv dvc vtbxs slsdck nkrpv gmstmh bmpt zmdl vnrnn dgtxv bghtk mdghb vhchv lfrt ptrj dnjt gfgmz vxmvk smxk bdsxbd thvbm qxst tlqtz zdfl dhhf mk dhlmfn rrlszk njpx sxtfvrhl hfdxb ztbgdkd zczv zhdtt xkvgj txvg slgnk lgfmt hqqzdc tvmpdqp rlh ffmq jrnqx gnbxs pxxjmds mhtc lvtrmn (contains sesame, eggs)
      |zvmnd kkmvg jgkvfl pbqn grzdp svbvqlq gnbxs mdlr rkrqrk szsdbv kllgt bmttzd rzvj qpktd vhchv srvzpc ncgn crbbk hfdxb qftpx srgh mpzq njpx dhlmfn hjnx mhtc srj tkpqpb ndqgjs jvjtrxt jtrrnz jrnqx sxtfvrhl snlhkm bfrj khqvlxn mhzbkt dgtxv jrs slsqp cxzrtx bghtk rtpgqd zczv nfff zjlxft zxstb xbjqr cnzz dfdr dxbmdj lcplblp qnhvn lfqhssm bdclf ffmq hbfnkq mjpqlvj gmstmh fxzgqn jnqgtrq lgfmt jflxtnp (contains peanuts, sesame)
      |crbbk hjnx thd ljclf ttvtr xkvgj slgnk srvzpc rfnzl tvmpdqp txvg fqll cjdcn mpzq crrgj vmfhv zhdtt sstvx ztr zxstb ljvx dctgv kcvzks jmc zngdbgf cgrjf kkdzxr rknm jnqgtrq xhxrg zjlxft cnzz xxgd lbzg dfxfqs jgjx jrnqx gmstmh dfdr lfrt snlhkm khqvlxn gnbxs gvtvmn zgcncf svbvqlq mhtc hfdxb dhhf hbfnkq bmttzd jvd (contains sesame)
      |jfkhrp dhhf slsqp jtrrnz mjpqlvj mdlr zmdl sdts mpzq tzxmtx bhrv nvrndt nzdmcf flrn tkpqpb krgmpn bmttzd zngdbgf nfff zxstb jrs khqvlxn jflxtnp tfshb slgnk ltnqh rdnsghf hjnx fstx hkchvb xbjqr fdknsl ljvx rlh kkdzxr smxk hbfnkq jgkvfl thzbt mhtc xdfjsm lfqhssm hhrdrl zvmnd gnbxs srgh rtpgqd sqrk jrnqx gmstmh rfnzl xhxrg njpx qmbn rknm fqgl rgpvhb lcplblp qxst qjhmth bmxlldv fvmzmrd ndqgjs zjlxft qftpx qpktd jmc mcmrzs slgstf rkhs dgtxv hfdxb nkvdvs (contains peanuts, nuts, eggs)
      |vhchv jtrrnz kkmvg bdsxbd zxstb kcvzks rxkt gmstmh sqrk mtlcq gnbxs lcplblp qftpx qnhvn drcj zmdl tvmpdqp nfff ljvx mhtc cmfjr dvc kllgt txvg qnvml slfpcp stjr pdbcdp fqll zpcqb slgnk zczv ljfsmv jrnqx fmkmrk rrlszk rlh xqnrcg dctgv srvzpc flrn zgcncf vtbxs lvtrmn hbfnkq xxgd hqqzdc jlfmg snlhkm mcmrzs xhxrg fmj cqvqlp bfrj srj sxtfvrhl (contains nuts)
      |nnxxsjh slgstf srj mhtc prkc njpx thd tzxmtx hkchvb fqgl zxstb mdlr qxc fmkmrk krgmpn bmttzd kllgt zgcncf cmfjr nzdmcf rdnsghf ptrj mxprt gfgnx lgcrd txvg snlhkm ljfsmv mxsbp vxmvk ffmq bdsxbd stjr pcxzx qnvml kkdzxr jxj jrnqx sdts dctgv dvffd fdknsl cnzz flrn qdths crrgj fstx fbkpkv zpvkl slsdck hqqzdc jflxtnp rxkt kclh bhrv gfgmz dfdr jvjtrxt lgfmt pbqn ljvx bghtk jvd xdfjsm kkmvg gnbxs ttvtr bfrj tlst jgkvfl cxzrtx hbfnkq (contains dairy, nuts)
      |jgkvfl bhrv ztbgdkd mcmrzs mk thvbm gnbxs xbjqr kllgt thd sstvx zxstb cjdcn jnqgtrq hbfnkq slfpcp jrnqx szsdbv hfdxb rlh xdfjsm lvtrmn kcvzks rknm mhzbkt hhrdrl tvmpdqp ptrj slsdck lgcrd zhdtt bmttzd slsqp lcplblp rrlszk slgstf rzvj crrgj hqqzdc gqsnd jtrrnz ljclf zpp crbbk ndqgjs mhtc tnd qlcvd npvv srj smxk rkrqrk hgndp (contains sesame, dairy)
      |rlh fdrgkrx mxprt jvd thd jflxtnp mtlcq rgpvhb rdnsghf svbvqlq flrn zhdtt zjlxft jtrrnz mjpqlvj hqqzdc mdghb mhzbkt bmpt lgfmt crrgj tkpqpb slgnk smxk lgcrd rltxnqf bhrv qdths vnrnn slsdck zngdbgf fstx jrnqx hbfnkq cgrjf gnbxs bmxlldv xdfjsm ljvx ztbgdkd dfdr nvrndt sxtfvrhl qmbn snlhkm nkrpv bdclf fqll qjhmth zvmnd tlst tcxz ttvtr xqnrcg grzdp mhtc jnqgtrq tvmpdqp dhlmfn rxkt xxgd kdxht nzdmcf xbjqr ndqgjs ptrj dctgv thvbm pcxzx zgcncf zdfl zmdl dxbmdj cxzrtx hgndp nfff fmj hkchvb bghtk kllgt cqvqlp fqgl zxstb txvg (contains eggs, dairy)
      |zpcqb crrgj rkhs jtrrnz lvtrmn mhtc vtbxs thtsvz vhchv rxkt lfqhssm ljvx xqnrcg dvc hkchvb thd sdts nkvdvs jvjtrxt drcj rxhv bhrv slsdck tlqtz gvtvmn cnzz ndqgjs qxst hbfnkq pcxzx kllgt hqqzdc fnfm smxk mxsbp bmpt jgjx tzxmtx jtnfbh jflxtnp khqvlxn zngdbgf slfpcp szsmh ptrj zjlxft fdrgkrx kclh dfdr rknm fbkpkv zpp jrnqx nkrpv vmfhv mdghb zgcncf qlcvd srgh xxgd mk kpn gnbxs kcvzks dnjt rlh hjnx kkmvg rrlszk lbzg tfshb npvv bmxlldv zxstb zhdtt rkrqrk zpvkl zbkjjkz zvmnd xdfjsm (contains eggs, fish, dairy)
      |mhtc hbfnkq jxj bhrv ljclf hfdxb jflxtnp njpx tzxmtx khqvlxn prkc cnzz bdclf kllgt cgrjf vxmvk xbjqr qnvml rltxnqf ljvx rkrqrk nltxdhr rlh dfxfqs gfgmz ndqgjs jrnqx qftpx fvmzmrd vhchv snlhkm srj ljfsmv dgtxv xxgd kclh dvffd szsdbv lgfmt zpp rxkt hjnx mxsbp thd krgmpn sqrk fbkpkv zxstb kpn hhrdrl dvc mpzq lbzg qdths ltnqh lfqhssm gvtvmn mhzbkt zngdbgf jlfmg (contains wheat)
      |jtnfbh mdlr cqvqlp kllgt hfdxb cmfjr vmfhv mjpqlvj rrlszk zpp jmc nfff srgh jrnqx xszpg qnvml slgstf hbfnkq drcj kpn rlh lbzg dvc mhtc hvkvz zdfl fbkpkv cnzz fqll zxstb dfxfqs vxmvk dhlmfn pcxzx ztr ffmq jvjtrxt cjdcn sstvx xkvgj jlfmg gnbxs bdsxbd cxzrtx kkdzxr snlhkm pdbcdp (contains sesame)
      |jrnqx qnhvn khqvlxn jlfmg kdxht rgpvhb cqvqlp qdths xszpg lgfmt ndqgjs grzdp hfdxb rkrqrk nltxdhr npvv krgmpn pbqn mhtc tzxmtx kclh dxbmdj fvmzmrd srgh mpzq kkmvg pcxzx fstx gnbxs slgnk jrs ttvtr ljclf nkrpv dvc bfrj kllgt rkhs thzbt qnvml fdknsl cmfjr hhrdrl hgndp xqnrcg jnqgtrq hbfnkq drcj tkpqpb gqsnd fmkmrk thd slgstf rknm nfff qxc vhchv kpn ljvx ptrj jflxtnp flrn qjhmth lvtrmn nzdmcf tlst fqll (contains sesame, dairy)
      |flrn rltxnqf cqvqlp gqsnd bghtk krgmpn jlfmg hfdxb xbjqr zjlxft fstx ljvx kllgt qxc npvv jxj jgjx srgh cmfjr mhtc dhlmfn sqrk svbvqlq tfshb zxstb vmfhv hbfnkq jmc jvjtrxt xkvgj kkmvg cxzrtx sstvx stjr ffmq thtsvz jrnqx zbkjjkz tkpqpb (contains nuts)
      |xhxrg hbfnkq kllgt hjnx mk nkvdvs dvc jnqgtrq lgfmt mhtc dxbmdj bdclf grzdp qnvml ljfsmv cqvqlp kclh rrlszk xxgd xbjqr thvbm smxk zbkjjkz fbkpkv rkhs mtlcq qxc gnbxs njpx bhrv snlhkm ljvx jrnqx zpcqb zxstb tcxz thd vxmvk slsqp ljclf txvg krgmpn jflxtnp rgpvhb (contains sesame, fish)
      |ljvx thvbm grzdp dfdr tfshb dgtxv ztr dhhf gvtvmn ndqgjs mdlr rlh zdfl zngdbgf zxstb mhtc rknm zpxvch jfkhrp hfdxb mjpqlvj nnxxsjh njpx lfrt nzdmcf jnqgtrq gqsnd gmstmh hbfnkq lcplblp jtnfbh khqvlxn sxtfvrhl slsdck crrgj jrnqx rrlszk thd slfpcp zvmnd mpzq pbqn hgndp stjr txvg gfgnx cjdcn xszpg kllgt mhzbkt kpn rfnzl (contains soy, eggs, dairy)
      |dvc tkpqpb vtbxs zbkjjkz bmxlldv pbqn rknm bhrv jflxtnp jmc kclh zvmnd dctgv hfdxb sstvx gnbxs zdfl hhrdrl tfshb mxsbp rgpvhb qftpx npvv slsqp dgtxv hjnx mhtc hqqzdc mxprt szsdbv xdfjsm gfgnx xhxrg pdbcdp qmbn zxstb jtrrnz thvbm ndqgjs svbvqlq gvtvmn drcj fvmzmrd zhdtt bfrj cjdcn zpcqb zjlxft slgstf rdnsghf lbzg cgrjf lgcrd hbfnkq ljclf rxkt thd rrlszk fdrgkrx smxk kpn qnhvn ttvtr bghtk jfkhrp thzbt jgjx ztr snlhkm jrnqx flrn pxxjmds njpx grzdp sdts zgcncf jrs mjpqlvj ljvx fmj dxbmdj jgkvfl (contains fish, eggs)
      |jrs xhxrg ljvx hbfnkq dfxfqs zpp mhzbkt mhtc lgfmt bhrv zpxvch dxbmdj kllgt ttvtr dctgv dfdr prkc gnbxs kdxht ffmq qpktd fvmzmrd gfgmz stjr vhchv thtsvz vnrnn kpn zxstb sstvx hjnx xxgd krgmpn qftpx rtpgqd thvbm fstx jrnqx tzxmtx ncgn svbvqlq slgstf rdnsghf qmbn tvmpdqp hgndp dhlmfn xkvgj jxj (contains wheat, nuts, dairy)
      |cgrjf hfdxb vtbxs qpktd zngdbgf bhrv kclh jvd zdfl nnxxsjh nzdmcf fmj cqvqlp zmdl kkdzxr xqnrcg rzvj ljvx thzbt nkrpv lcplblp kllgt rlh tkpqpb dxbmdj mhtc sxtfvrhl tzxmtx thvbm slgstf zpvkl zxstb txvg jflxtnp hbfnkq ljfsmv zpcqb jmc jrnqx dvffd jgkvfl xszpg dgtxv cxzrtx khqvlxn fbkpkv zpxvch tfshb slfpcp lgfmt smxk dnjt jfkhrp bdsxbd bfrj pxxjmds gfgnx drcj (contains dairy, soy, fish)
      |slgnk qftpx hfdxb qjhmth nfff fmkmrk qlcvd tlst fxzgqn ndqgjs lbzg rxhv lfqhssm gvtvmn jtnfbh ltnqh tnd gfgmz ttvtr rdnsghf rkhs kllgt mhtc hbfnkq tfshb nnxxsjh mjpqlvj rrlszk hqqzdc fstx jvd mk lfrt thvbm kpn grzdp rkrqrk dhhf zngdbgf txvg njpx zxstb vnrnn krgmpn srvzpc nzdmcf mpzq bmttzd dgtxv hvkvz rtpgqd srj vmfhv jrnqx dnjt cnzz nkvdvs vhchv xszpg ptrj zpxvch qmbn svbvqlq kdxht kkdzxr npvv stjr fbkpkv xqnrcg slfpcp snlhkm jlfmg zhdtt bfrj thzbt mtlcq fnfm fmj xdfjsm zgcncf jmc pxxjmds gnbxs qxst dfxfqs (contains eggs, soy)
      |tfshb rxkt tlqtz fmkmrk mtlcq dxbmdj zxstb rdnsghf fnfm qjhmth zpvkl jrnqx njpx cmfjr svbvqlq mhtc hhrdrl ptrj cjdcn vmfhv gfgmz hfdxb ffmq nnxxsjh mxprt gnbxs zpcqb rkrqrk szsmh hbfnkq bmttzd cnzz fvmzmrd kkmvg zdfl kllgt zjlxft thd dfdr prkc kclh (contains wheat, dairy, peanuts)
      |""".stripMargin
