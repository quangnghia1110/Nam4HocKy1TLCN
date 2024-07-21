package studentConsulting.repository.questionAnswer;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.questionAnswer.AnswerEntity;

public interface AnswerRepository extends  JpaRepository<AnswerEntity, Integer>{

}
