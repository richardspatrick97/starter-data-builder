package dev.ikm.tinkar.starterdata;

import com.fasterxml.uuid.Generators;
import com.fasterxml.uuid.impl.NameBasedGenerator;

import java.util.UUID;

public class UUIDUtility {

    private final NameBasedGenerator uuidGenerator;

    public UUIDUtility() {
        this.uuidGenerator = Generators.nameBasedGenerator();
    }

    public UUIDUtility(UUID namespace) {
        this.uuidGenerator = Generators.nameBasedGenerator(namespace);
    }

    public UUID createUUID(String seed){
        return uuidGenerator.generate(seed);
    }

    public UUID createUUID(){
        return UUID.randomUUID();
    }

}
