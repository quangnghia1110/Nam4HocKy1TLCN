package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public interface ClassRepository extends  JpaRepository<QuestionEntity, Integer>{

}
