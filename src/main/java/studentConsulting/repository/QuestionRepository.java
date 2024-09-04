package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public interface QuestionRepository extends JpaRepository<QuestionEntity, Integer>{

}
