package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public interface ClassUserRepository extends  JpaRepository<QuestionEntity, Integer>{

}
