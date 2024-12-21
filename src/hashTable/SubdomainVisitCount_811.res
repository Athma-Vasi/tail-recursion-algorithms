let subdomainVisitCount = (cpdomains: array<string>) => {
  let rec countPairedDomains = (domains: array<string>, index: int) => {
    switch index === Array.length(cpdomains) {
    | true => domains
    | false => {
        let cpdomain =
          cpdomains->Array.at(index)->Option.map(cpd => cpd)->Option.getOr(String.make())
        let visits =
          cpdomain
          ->String.split("")
          ->Array.at(0)
          ->Option.map(t => t)
          ->Option.getOr("-1")
        let domains =
          cpdomain
          ->String.split("")
          ->Array.at(1)
          ->Option.map(t => t)
          ->Option.getOr(String.make())
        let subdomains = domains->String.split(".")
      }
    }
  }
}
