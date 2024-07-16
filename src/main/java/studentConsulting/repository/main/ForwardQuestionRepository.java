package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.entity.main.QuestionEntity;

public interface ForwardQuestionRepository extends  JpaRepository<QuestionEntity, Integer>{

}
