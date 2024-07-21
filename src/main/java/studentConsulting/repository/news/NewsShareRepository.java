package studentConsulting.repository.news;

import org.springframework.data.jpa.repository.JpaRepository;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public interface NewsShareRepository extends  JpaRepository<QuestionEntity, Integer>{

}
