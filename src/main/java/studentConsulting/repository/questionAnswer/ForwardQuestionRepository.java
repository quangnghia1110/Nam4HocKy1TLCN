package studentConsulting.repository.questionAnswer;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.questionAnswer.ForwardQuestionEntity;

public interface ForwardQuestionRepository extends  JpaRepository<ForwardQuestionEntity, Integer>{

}
