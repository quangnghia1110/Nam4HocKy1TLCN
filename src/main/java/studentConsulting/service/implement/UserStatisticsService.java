package studentConsulting.service.implement;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.payload.dto.UserStatisticsDTO;
import studentConsulting.repository.ConsultationScheduleRepository;
import studentConsulting.repository.ConversationRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.RatingRepository;
import studentConsulting.repository.UserStatisticsRepository;
import studentConsulting.specification.ConsultationScheduleSpecification;
import studentConsulting.specification.ConversationSpecification;
import studentConsulting.specification.QuestionSpecification;
import studentConsulting.specification.RatingSpecification;

@Service
public class UserStatisticsService {

    @Autowired
    private UserStatisticsRepository userStatisticsRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private RatingRepository ratingRepository;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;
    
    public UserStatisticsDTO getUserStatistics(Integer userId) {
        UserStatisticsDTO statistics = new UserStatisticsDTO();
        statistics.setTotalQuestions(userStatisticsRepository.countTotalQuestionsByUser(userId));
        statistics.setQuestionsOver500Views(userStatisticsRepository.countQuestionsWithMoreThan500Views(userId));
        statistics.setTotalAppointments(userStatisticsRepository.countConsultationSchedulesByUser(userId));
        statistics.setTotalRatings(userStatisticsRepository.countRatingsByUser(userId));
        return statistics;
    }

    public List<Map<String, Object>> getQuestionsByStatusAndTimeFrame(Integer userId, String statusKey, LocalDate startDate, LocalDate endDate) {
        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.hasUserQuestion(userId));

        if (statusKey != null && !statusKey.isEmpty()) {
            QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(statusKey);
            spec = spec.and(QuestionSpecification.hasStatus(filterStatus));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        List<QuestionEntity> questionEntities = questionRepository.findAll(spec);

        return questionEntities.stream()
                .map(question -> Map.<String, Object>of(
                        "year", question.getCreatedAt().getYear(),
                        "month", question.getCreatedAt().getMonthValue(),
                        "day", question.getCreatedAt().getDayOfMonth(),
                        "question", questionEntities.size() 
                ))
                .collect(Collectors.toList());
    }

    
    public List<Map<String, Object>> getRatingsByTimeFrame(Integer userId, LocalDate startDate, LocalDate endDate) {
        Specification<RatingEntity> spec = Specification.where(RatingSpecification.hasUser(userId));

        if (startDate != null && endDate != null) {
            spec = spec.and(RatingSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(RatingSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(RatingSpecification.hasDateBefore(endDate));
        }

        List<RatingEntity> ratingEntities = ratingRepository.findAll(spec);

        return ratingEntities.stream()
                .map(rating -> Map.<String, Object>of(
                        "year", rating.getSubmittedAt().getYear(),
                        "month", rating.getSubmittedAt().getMonthValue(),
                        "day", rating.getSubmittedAt().getDayOfMonth(),
                        "rating", ratingEntities.size()
                ))
                .collect(Collectors.toList());
    }
    
    public List<Map<String, Object>> getConsultationSchedulesByTimeFrame(Integer userId, LocalDate startDate, LocalDate endDate) {
        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasUser(userId));

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDateBefore(endDate));
        }

        List<ConsultationScheduleEntity> appointmentEntities = consultationScheduleRepository.findAll(spec);

        return appointmentEntities.stream()
                .map(appointment -> Map.<String, Object>of(
                        "year", appointment.getConsultationDate().getYear(),
                        "month", appointment.getConsultationDate().getMonthValue(),
                        "day", appointment.getConsultationDate().getDayOfMonth(),
                        "consultationSchedule", appointmentEntities.size()
                ))
                .collect(Collectors.toList());
    }
    
    @Autowired
    private ConversationRepository conversationRepository;

    public List<Map<String, Object>> getConversationsByTimeFrame(Integer userId, LocalDate startDate, LocalDate endDate) {
        Specification<ConversationEntity> spec = Specification.where(ConversationSpecification.isOwner(userId));

        if (startDate != null && endDate != null) {
            spec = spec.and(ConversationSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConversationSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConversationSpecification.hasDateBefore(endDate));
        }

        List<ConversationEntity> conversationEntities = conversationRepository.findAll(spec);

        return conversationEntities.stream()
                .map(conversation -> Map.<String, Object>of(
                        "year", conversation.getCreatedAt().getYear(),
                        "month", conversation.getCreatedAt().getMonthValue(),
                        "day", conversation.getCreatedAt().getDayOfMonth(),
                        "conversation", conversationEntities.size()
                ))
                .collect(Collectors.toList());
    }
    
    public List<Map<String, Object>> getQuestionsByDepartmentAndField(Integer userId, Integer departmentId, Integer fieldId, LocalDate startDate, LocalDate endDate) {
        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.hasUserQuestion(userId));

        if (departmentId != null) {
            spec = spec.and(QuestionSpecification.hasDepartments(departmentId));
        }

        if (fieldId != null) {
            spec = spec.and(QuestionSpecification.hasFieldInDepartment(fieldId, departmentId));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        List<QuestionEntity> questionEntities = questionRepository.findAll(spec);

        return questionEntities.stream()
                .map(question -> Map.<String, Object>of(
                        "year", question.getCreatedAt().getYear(),
                        "month", question.getCreatedAt().getMonthValue(),
                        "day", question.getCreatedAt().getDayOfMonth(),
                        "department", question.getDepartment().getName(),
                        "field", question.getField().getName(),
                        "count", questionEntities.size()
                ))
                .collect(Collectors.toList());
    }
}
