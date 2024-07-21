package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public interface AnswerRepository extends  JpaRepository<QuestionEntity, Integer>{

}
