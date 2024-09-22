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
import studentConsulting.model.entity.news.PostEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.payload.dto.ConsultantStatisticsDTO;
import studentConsulting.model.payload.dto.UserStatisticsDTO;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.ConsultationScheduleRepository;
import studentConsulting.repository.ConversationRepository;
import studentConsulting.repository.PostRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.RatingRepository;
import studentConsulting.repository.StatisticsRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.specification.AnswerSpecification;
import studentConsulting.specification.ConsultationScheduleSpecification;
import studentConsulting.specification.ConversationSpecification;
import studentConsulting.specification.PostSpecification;
import studentConsulting.specification.QuestionSpecification;
import studentConsulting.specification.RatingSpecification;

@Service
public class StatisticsService {

    @Autowired
    private StatisticsRepository statisticsRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private RatingRepository ratingRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;
    
    @Autowired
    private PostRepository postRepository;
    
    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;
    
    public UserStatisticsDTO getUserStatistics(Integer userId) {
        UserStatisticsDTO statistics = new UserStatisticsDTO();
        statistics.setTotalQuestions(statisticsRepository.countTotalQuestionsByUser(userId));
        statistics.setQuestionsOver500Views(statisticsRepository.countQuestionsWithMoreThan500Views(userId));
        statistics.setTotalAppointments(statisticsRepository.countConsultationSchedulesByUser(userId));
        statistics.setTotalRatings(statisticsRepository.countRatingsByUser(userId));
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

        Map<String, Long> groupedData = questionEntities.stream()
            .collect(Collectors.groupingBy(question -> 
                question.getCreatedAt().getYear() + "-" +
                question.getCreatedAt().getMonthValue() + "-" +
                question.getCreatedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
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

        Map<String, Long> groupedData = ratingEntities.stream()
            .collect(Collectors.groupingBy(rating -> 
                rating.getSubmittedAt().getYear() + "-" +
                rating.getSubmittedAt().getMonthValue() + "-" +
                rating.getSubmittedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
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

        Map<String, Long> groupedData = appointmentEntities.stream()
            .collect(Collectors.groupingBy(appointment -> 
                appointment.getConsultationDate().getYear() + "-" +
                appointment.getConsultationDate().getMonthValue() + "-" +
                appointment.getConsultationDate().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
            .collect(Collectors.toList());
    }

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

        Map<String, Long> groupedData = conversationEntities.stream()
            .collect(Collectors.groupingBy(conversation -> 
                conversation.getCreatedAt().getYear() + "-" +
                conversation.getCreatedAt().getMonthValue() + "-" +
                conversation.getCreatedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
            .collect(Collectors.toList());
    }

    
    public List<Map<String, Object>> getConversationsMemberByTimeFrame(Integer userId, LocalDate startDate, LocalDate endDate) {
        Specification<ConversationEntity> spec = Specification.where(ConversationSpecification.isMember(userId));

        if (startDate != null && endDate != null) {
            spec = spec.and(ConversationSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConversationSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConversationSpecification.hasDateBefore(endDate));
        }

        List<ConversationEntity> conversationEntities = conversationRepository.findAll(spec);

        Map<String, Long> groupedData = conversationEntities.stream()
            .collect(Collectors.groupingBy(conversation -> 
                conversation.getCreatedAt().getYear() + "-" +
                conversation.getCreatedAt().getMonthValue() + "-" +
                conversation.getCreatedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
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

        Map<String, Long> groupedData = questionEntities.stream()
            .collect(Collectors.groupingBy(question -> 
                question.getCreatedAt().getYear() + "-" +
                question.getCreatedAt().getMonthValue() + "-" +
                question.getCreatedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "department", questionEntities.stream()
                        .filter(q -> q.getCreatedAt().getYear() == Integer.parseInt(parts[0]) &&
                                     q.getCreatedAt().getMonthValue() == Integer.parseInt(parts[1]) &&
                                     q.getCreatedAt().getDayOfMonth() == Integer.parseInt(parts[2]))
                        .findFirst().get().getDepartment().getName(),
                    "field", questionEntities.stream()
                        .filter(q -> q.getCreatedAt().getYear() == Integer.parseInt(parts[0]) &&
                                     q.getCreatedAt().getMonthValue() == Integer.parseInt(parts[1]) &&
                                     q.getCreatedAt().getDayOfMonth() == Integer.parseInt(parts[2]))
                        .findFirst().get().getField().getName(),
                    "count", entry.getValue()
                );
            })
            .collect(Collectors.toList());
    }

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    public ConsultantStatisticsDTO getConsultantStatistics(Integer consultantId) {
        ConsultantStatisticsDTO statistics = new ConsultantStatisticsDTO();
        LocalDate today = LocalDate.now();
        Integer departmentId = userRepository.findDepartmentIdByConsultantId(consultantId); 
        statistics.setTotalQuestionsInDay(statisticsRepository.countQuestionsByDepartmentIdAndDate(departmentId, today));
                statistics.setTotalForwardedQuestions(statisticsRepository.countDistinctToDepartmentsByConsultantIdAndStatusForwardedTrue(consultantId));
        statistics.setTotalDeletedQuestions(statisticsRepository.countByConsultantIdAndDeletedTrue(consultantId));
        statistics.setTotalAnswersGiven(statisticsRepository.countByConsultantIdAndAnsweredTrue(consultantId));
        statistics.setTotalAnswerApproval(statisticsRepository.countByConsultantIdAndStatusApprovalTrue(consultantId));
        statistics.setTotalConsultantSchedule(statisticsRepository.countByConsultantIdAndStatusConfirmedTrue(consultantId));
        statistics.setTotalApprovedPosts(statisticsRepository.countByConsultantIdAndPublishedTrue(consultantId));
        statistics.setTotalConversations(statisticsRepository.countByConsultantId(consultantId));
        return statistics;
    }
    
    public List<Map<String, Object>> getDeletedQuestionsByTimeFrame(Integer consultantId, LocalDate startDate, LocalDate endDate) {
        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.isDeletedByConsultant(consultantId));

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        List<QuestionEntity> questionEntities = questionRepository.findAll(spec);

        Map<String, Long> groupedData = questionEntities.stream()
            .collect(Collectors.groupingBy(question -> 
                question.getCreatedAt().getYear() + "-" +
                question.getCreatedAt().getMonthValue() + "-" +
                question.getCreatedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
            .collect(Collectors.toList());
    }

    public List<Map<String, Object>> getAnswersGivenByTimeFrame(Integer consultantId, LocalDate startDate, LocalDate endDate) {
        Specification<AnswerEntity> spec = Specification.where(AnswerSpecification.isAnsweredByConsultant(consultantId));

        if (startDate != null && endDate != null) {
            spec = spec.and(AnswerSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(AnswerSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(AnswerSpecification.hasDateBefore(endDate));
        }

        List<AnswerEntity> answerEntities = answerRepository.findAll(spec);

        Map<String, Long> groupedData = answerEntities.stream()
            .collect(Collectors.groupingBy(answer -> 
                answer.getCreatedAt().getYear() + "-" +
                answer.getCreatedAt().getMonthValue() + "-" +
                answer.getCreatedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
            .collect(Collectors.toList());
    }

    public List<Map<String, Object>> getAnswerApprovalByTimeFrame(Integer consultantId, LocalDate startDate, LocalDate endDate) {
        Specification<AnswerEntity> spec = Specification.where(AnswerSpecification.isPendingApproval(consultantId));

        if (startDate != null && endDate != null) {
            spec = spec.and(AnswerSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(AnswerSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(AnswerSpecification.hasDateBefore(endDate));
        }

        List<AnswerEntity> answerEntities = answerRepository.findAll(spec);

        Map<String, Long> groupedData = answerEntities.stream()
            .collect(Collectors.groupingBy(answer -> 
                answer.getCreatedAt().getYear() + "-" +
                answer.getCreatedAt().getMonthValue() + "-" +
                answer.getCreatedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
            .collect(Collectors.toList());
    }


    public List<Map<String, Object>> getConsultationSchedulesConsultantByTimeFrame(Integer consultantId, LocalDate startDate, LocalDate endDate) {
        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasConsultant(consultantId));

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDateBefore(endDate));
        }

        List<ConsultationScheduleEntity> consultationScheduleEntities = consultationScheduleRepository.findAll(spec);

        Map<String, Long> groupedData = consultationScheduleEntities.stream()
            .collect(Collectors.groupingBy(schedule -> 
                schedule.getConsultationDate().getYear() + "-" +
                schedule.getConsultationDate().getMonthValue() + "-" +
                schedule.getConsultationDate().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
            .collect(Collectors.toList());
    }

    public List<Map<String, Object>> getApprovedPostsByTimeFrame(Integer consultantId, LocalDate startDate, LocalDate endDate) {
        Specification<PostEntity> spec = Specification.where(PostSpecification.isApprovedByConsultant(consultantId));

        if (startDate != null && endDate != null) {
            spec = spec.and(PostSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(PostSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(PostSpecification.hasDateBefore(endDate));
        }

        List<PostEntity> postEntities = postRepository.findAll(spec);

        Map<String, Long> groupedData = postEntities.stream()
            .collect(Collectors.groupingBy(post -> 
                post.getCreatedAt().getYear() + "-" +
                post.getCreatedAt().getMonthValue() + "-" +
                post.getCreatedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
            .collect(Collectors.toList());
    }


    public List<Map<String, Object>> getConversationsConsultantByTimeFrame(Integer consultantId, LocalDate startDate, LocalDate endDate) {
        Specification<ConversationEntity> spec = Specification.where(ConversationSpecification.hasConsultant(consultantId));

        if (startDate != null && endDate != null) {
            spec = spec.and(ConversationSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConversationSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConversationSpecification.hasDateBefore(endDate));
        }

        List<ConversationEntity> conversationEntities = conversationRepository.findAll(spec);

        Map<String, Long> groupedData = conversationEntities.stream()
            .collect(Collectors.groupingBy(conversation -> 
                conversation.getCreatedAt().getYear() + "-" +
                conversation.getCreatedAt().getMonthValue() + "-" +
                conversation.getCreatedAt().getDayOfMonth(), 
                Collectors.counting()
            ));

        return groupedData.entrySet().stream()
            .map(entry -> {
                String[] parts = entry.getKey().split("-");
                return Map.<String, Object>of(
                    "year", Integer.parseInt(parts[0]),
                    "month", Integer.parseInt(parts[1]),
                    "day", Integer.parseInt(parts[2]),
                    "count", entry.getValue()
                );
            })
            .collect(Collectors.toList());
    }


}
