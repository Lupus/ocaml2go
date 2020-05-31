
let run = (name, runner) => {
    Printf.printf("+++ %s tests\n%!", name);
    runner();
    Printf.printf("--- %s tests\n%!", name);
};

let () = {
    run("Bool", Bool_test.run);
    run("Int", Int_test.run);
    run("Option", Option_test.run);
    run("Result", Result_test.run);
    run("List", List_test.run);
    run("Stack", Stack_test.run);
    run("Queue", Queue_test.run);
    run("Set", Set_test.run);
    run("Map", Map_test.run);
    run("Fun", Fun_test.run);
    run("Buffer", Buffer_test.run);
};
