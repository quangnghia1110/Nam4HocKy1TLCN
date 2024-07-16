package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.entity.main.QuestionEntity;

public interface FaqRepository extends  JpaRepository<QuestionEntity, Integer>{

}
