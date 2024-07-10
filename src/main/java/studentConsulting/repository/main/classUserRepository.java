package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.entity.main.questionEntity;

public interface classUserRepository extends  JpaRepository<questionEntity, Integer>{

}
