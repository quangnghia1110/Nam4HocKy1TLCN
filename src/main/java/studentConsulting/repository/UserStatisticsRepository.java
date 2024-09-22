package studentConsulting.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import studentConsulting.model.entity.questionAnswer.QuestionEntity;

public interface UserStatisticsRepository extends JpaRepository<QuestionEntity, Integer>, JpaSpecificationExecutor<QuestionEntity> {

    @Query("SELECT COUNT(q) FROM QuestionEntity q WHERE q.user.id = :userId")
    Integer countTotalQuestionsByUser(@Param("userId") Integer userId);

    @Query("SELECT COUNT(q) FROM QuestionEntity q WHERE q.user.id = :userId AND q.views > 500")
    Integer countQuestionsWithMoreThan500Views(@Param("userId") Integer userId);

    @Query("SELECT COUNT(a) FROM ConsultationScheduleEntity a WHERE a.user.id = :userId")
    Integer countConsultationSchedulesByUser(@Param("userId") Integer userId);

    @Query("SELECT COUNT(r) FROM RatingEntity r WHERE r.user.id = :userId")
    Integer countRatingsByUser(@Param("userId") Integer userId);
}
