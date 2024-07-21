package studentConsulting.repository.main;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.main.QuestionEntity;

public interface FieldRepository extends  JpaRepository<QuestionEntity, Integer>{

}
