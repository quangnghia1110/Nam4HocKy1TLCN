package studentConsulting.service.implement.statistic;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.content.PostEntity;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.CommonQuestionEntity;
import studentConsulting.model.entity.question_answer.ForwardQuestionEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.rating.RatingEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.statistic.AdvisorStatisticsDTO;
import studentConsulting.model.payload.dto.statistic.ConsultantStatisticsDTO;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.communication.MessageRepository;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.content.PostRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.CommonQuestionRepository;
import studentConsulting.repository.question_answer.ForwardQuestionRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.rating.RatingRepository;
import studentConsulting.repository.statistic.StatisticsRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.statistic.IStatisticConsultantAdvisorAdminService;
import studentConsulting.specification.communication.ConversationSpecification;
import studentConsulting.specification.communication.MessageSpecification;
import studentConsulting.specification.consultation_schedule.ConsultationScheduleSpecification;
import studentConsulting.specification.content.PostSpecification;
import studentConsulting.specification.question_answer.AnswerSpecification;
import studentConsulting.specification.question_answer.CommonQuestionSpecification;
import studentConsulting.specification.question_answer.ForwardQuestionSpecification;
import studentConsulting.specification.question_answer.QuestionSpecification;
import studentConsulting.specification.rating.RatingSpecification;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.temporal.ChronoField;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class StatisticConsultantAdvisorAdminServiceImpl implements IStatisticConsultantAdvisorAdminService {

    @Autowired
    private StatisticsRepository statisticsRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private ForwardQuestionRepository forwardQuestionRepository;

    @Autowired
    private CommonQuestionRepository commonQuestionRepository;

    @Autowired
    private RatingRepository ratingRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private PostRepository postRepository;

    @Autowired
    private MessageRepository messageRepository;

    @Override
    public Object getStatisticsByRole(Integer userId, String role, LocalDate startDate, LocalDate endDate) {
        List<Integer> departmentIds;
        Integer consultantId = null;
        Integer departmentId;

        switch (role) {
            case SecurityConstants.Role.ADMIN:
                departmentIds = departmentRepository.findAllDepartmentIds();
                break;
            case SecurityConstants.Role.TRUONGBANTUVAN:
                departmentId = departmentRepository.findDepartmentByUserId(userId);
                return getAdvisorStatistics(Collections.singletonList(departmentId), consultantId, startDate, endDate);
            case SecurityConstants.Role.TUVANVIEN:
                departmentId = userRepository.findDepartmentIdByConsultantId(userId);
                consultantId = userId;
                return getConsultantStatistics(consultantId, departmentId, startDate, endDate);
            default:
                throw new Exceptions.ErrorException("Quyền không hợp lệ");
        }

        return getAdvisorStatistics(departmentIds, consultantId, startDate, endDate);
    }


    private AdvisorStatisticsDTO getAdvisorStatistics(List<Integer> departmentIds, Integer consultantId, LocalDate startDate, LocalDate endDate) {
        AdvisorStatisticsDTO statistics = new AdvisorStatisticsDTO();
        LocalDateTime startDateTime = (startDate != null) ? startDate.atStartOfDay() : null;
        LocalDateTime endDateTime = (endDate != null) ? endDate.atTime(LocalTime.MAX) : null;

        Timestamp startTimestamp = (startDateTime != null) ? Timestamp.valueOf(startDateTime) : null;
        Timestamp endTimestamp = (endDateTime != null) ? Timestamp.valueOf(endDateTime) : null;

        statistics.setTotalUniqueUsersAdvisedByMessages(
                statisticsRepository.countUniqueUsersAdvisedByDepartmentIds(departmentIds, startTimestamp, endTimestamp)
        );

        statistics.setTotalQuestions(statisticsRepository.countQuestionsByDepartmentIds(departmentIds, startDate, endDate));
        statistics.setTotalForwardedQuestions(statisticsRepository.countDistinctQuestionsForwarded(departmentIds, consultantId, startDate, endDate));
        statistics.setTotalDeletedQuestions(statisticsRepository.countDeletedQuestions(departmentIds, consultantId, startDate, endDate));
        statistics.setTotalAnswersGiven(statisticsRepository.countAnsweredQuestions(departmentIds, consultantId, startDate, endDate));
        statistics.setTotalAnswerApproval(statisticsRepository.countApprovedAnswers(departmentIds, consultantId, startDate, endDate));
        statistics.setTotalConfirmedConsultantSchedule(statisticsRepository.countConfirmedSchedules(departmentIds, consultantId, startDate, endDate));
        statistics.setTotalApprovedPosts(statisticsRepository.countApprovedPosts(consultantId, startDate, endDate));
        statistics.setTotalConversations(statisticsRepository.countConversations(departmentIds, consultantId, startDate, endDate));
        statistics.setTotalRatings(statisticsRepository.countRatingsByDepartmentIds(departmentIds, startDate, endDate));

        statistics.setTotalCommonQuestions(statisticsRepository.countCommonQuestionsByDepartmentIds(departmentIds, startDate, endDate));
        statistics.setTotalConsultants(statisticsRepository.countConsultantsByDepartmentIds(departmentIds, startDate, endDate));

        return statistics;
    }

    private ConsultantStatisticsDTO getConsultantStatistics(Integer consultantId, Integer departmentId, LocalDate startDate, LocalDate endDate) {
        ConsultantStatisticsDTO statistics = new ConsultantStatisticsDTO();

        LocalDateTime startDateTime = (startDate != null) ? startDate.atStartOfDay() : null;
        LocalDateTime endDateTime = (endDate != null) ? endDate.atTime(LocalTime.MAX) : null;

        Timestamp startTimestamp = (startDateTime != null) ? Timestamp.valueOf(startDateTime) : null;
        Timestamp endTimestamp = (endDateTime != null) ? Timestamp.valueOf(endDateTime) : null;


        statistics.setTotalUniqueUsersAdvisedByMessages(
                statisticsRepository.countUniqueUsersAdvisedByConsultant(consultantId, startTimestamp, endTimestamp)
        );

        statistics.setTotalQuestions(statisticsRepository.countQuestionsByDepartmentIds(List.of(departmentId), startDate, endDate));
        statistics.setTotalForwardedQuestions(statisticsRepository.countDistinctQuestionsForwarded(null, consultantId, startDate, endDate));
        statistics.setTotalDeletedQuestions(statisticsRepository.countDeletedQuestions(null, consultantId, startDate, endDate));
        statistics.setTotalAnswersGiven(statisticsRepository.countAnsweredQuestions(null, consultantId, startDate, endDate));
        statistics.setTotalAnswerApproval(statisticsRepository.countApprovedAnswers(null, consultantId, startDate, endDate));
        statistics.setTotalConfirmedConsultantSchedule(statisticsRepository.countConfirmedSchedules(null, consultantId, startDate, endDate));
        statistics.setTotalApprovedPosts(statisticsRepository.countApprovedPosts(consultantId, startDate, endDate));
        statistics.setTotalConversations(statisticsRepository.countConversations(null, consultantId, startDate, endDate));
        statistics.setTotalRatings(statisticsRepository.countRatingsByConsultantId(consultantId, startDate, endDate));

        return statistics;
    }


    @Override
    public List<Map<String, Object>> getDeletedQuestionsByYear(Integer departmentId, Integer year, Integer consultantId) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.isDeletedByConsultant())
                .and(QuestionSpecification.hasExactYear(year));

        if (departmentId != null) {
            spec = spec.and(QuestionSpecification.hasDepartmentId(departmentId));
        }

        if (consultantId != null) {
            spec = spec.and(QuestionSpecification.hasConsultantAnswer(consultantId));
        }

        List<QuestionEntity> questionEntities = questionRepository.findAll(spec);

        for (QuestionEntity question : questionEntities) {
            int month = question.getCreatedAt().get(ChronoField.MONTH_OF_YEAR);
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getForwardQuestionsByYear(Integer consultantId, Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<ForwardQuestionEntity> spec = ForwardQuestionSpecification.hasExactYear(year);

        if (departmentId != null) {
            spec = spec.and(
                    ForwardQuestionSpecification.hasFromDepartment(departmentId)
                            .or(ForwardQuestionSpecification.hasToDepartment(departmentId))
            );
        }

        if (consultantId != null) {
            spec = spec.and(ForwardQuestionSpecification.hasCreatedBy(consultantId));
        }

        List<ForwardQuestionEntity> forwardQuestions = forwardQuestionRepository.findAll(spec);

        for (ForwardQuestionEntity question : forwardQuestions) {
            int month = question.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }


    @Override
    public List<Map<String, Object>> getAnswersGivenByRole(Integer consultantId, Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<AnswerEntity> spec = Specification.where(AnswerSpecification.hasExactYear(year));

        if (consultantId != null) {
            spec = spec.and(AnswerSpecification.isAnsweredByConsultant(consultantId));
        } else if (departmentId != null) {
            spec = spec.and(AnswerSpecification.hasDepartment(departmentId));
        } else {
        }

        List<AnswerEntity> answerEntities = answerRepository.findAll(spec);

        for (AnswerEntity answer : answerEntities) {
            int month = answer.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }


    @Override
    public List<Map<String, Object>> getAnswerApprovalByRole(Integer consultantId, Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<AnswerEntity> spec = Specification.where(AnswerSpecification.hasExactYear(year));

        if (consultantId != null) {
            spec = spec.and(AnswerSpecification.isPendingApproval(consultantId));
        } else {
            spec = spec.and(AnswerSpecification.isApproved());
            if (departmentId != null) {
                spec = spec.and(AnswerSpecification.hasDepartment(departmentId));
            }
        }

        List<AnswerEntity> answerEntities = answerRepository.findAll(spec);

        for (AnswerEntity answer : answerEntities) {
            int month = answer.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getConsultationSchedulesByRole(Integer consultantId, Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasExactYear(year));

        if (consultantId != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasConsultant(consultantId));
        } else if (departmentId != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDepartment(departmentId));
        } else {
        }

        List<ConsultationScheduleEntity> consultationScheduleEntities = consultationScheduleRepository.findAll(spec);

        for (ConsultationScheduleEntity schedule : consultationScheduleEntities) {
            int month = schedule.getConsultationDate().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }


    @Override
    public List<Map<String, Object>> getConversationsByRole(Integer consultantId, Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<ConversationEntity> spec = Specification.where(ConversationSpecification.hasExactYear(year));

        if (consultantId != null) {
            spec = spec.and(ConversationSpecification.hasConsultantAsMember(consultantId));
        } else if (departmentId != null) {
            spec = spec.and(ConversationSpecification.hasDepartment(departmentId));
        }

        List<ConversationEntity> conversationEntities = conversationRepository.findAll(spec);

        for (ConversationEntity conversation : conversationEntities) {
            int month = conversation.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }


    @Override
    public List<Map<String, Object>> getRatingsByRole(Integer consultantId, Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<RatingEntity> spec = Specification.where(RatingSpecification.hasExactYear(year));

        if (consultantId != null) {
            spec = spec.and(RatingSpecification.hasConsultantId(consultantId));
        } else if (departmentId != null) {
            spec = spec.and(RatingSpecification.hasDepartment(departmentId));
        }

        List<RatingEntity> ratingEntities = ratingRepository.findAll(spec);

        for (RatingEntity rating : ratingEntities) {
            int month = rating.getSubmittedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getApprovedAndPendingPostsByYear(Integer consultantId, Integer departmentId, Integer year) {
        Map<Integer, Map<String, Object>> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            Map<String, Object> counts = new HashMap<>();
            counts.put("approved", 0L);
            counts.put("pending", 0L);
            monthlyCount.put(i, counts);
        }

        Specification<PostEntity> spec = Specification.where(PostSpecification.hasExactYear(year));

        if (consultantId != null) {
            spec = spec.and(PostSpecification.isApprovedByConsultant(consultantId));
        }

        List<PostEntity> postEntities = postRepository.findAll(spec);

        for (PostEntity post : postEntities) {
            int month = post.getCreatedAt().getMonthValue();
            boolean isApproved = post.isApproved();

            Map<String, Object> counts = monthlyCount.get(month);
            counts.put(isApproved ? "approved" : "pending", (Long) counts.get(isApproved ? "approved" : "pending") + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "approved", entry.getValue().get("approved"),
                        "pending", entry.getValue().get("pending")))
                .collect(Collectors.toList());
    }


    @Override
    public List<Map<String, Object>> getCommonQuestionsByYear(Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<CommonQuestionEntity> spec = Specification.where(CommonQuestionSpecification.hasExactYear(year))
                .and(CommonQuestionSpecification.hasDepartment(departmentId));

        List<CommonQuestionEntity> commonQuestionEntities = commonQuestionRepository.findAll(spec);

        for (CommonQuestionEntity commonQuestion : commonQuestionEntities) {
            int month = commonQuestion.getCreatedAt().getMonthValue();
            monthlyCount.put(month, monthlyCount.get(month) + 1);
        }

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

    @Override
    public List<Map<String, Object>> getUniqueUsersAdvisedByMessages(Integer consultantId, Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<MessageEntity> spec = Specification.where(MessageSpecification.hasExactYear(year));

        if (consultantId != null) {
            spec = spec.and(MessageSpecification.isSentByConsultant(consultantId));
        } else if (departmentId != null) {
            spec = spec.and(MessageSpecification.isSentByDepartment(departmentId));
        }

        List<MessageEntity> messages = messageRepository.findAll(spec);

        Map<Integer, Set<Integer>> uniqueUsersPerMonth = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            uniqueUsersPerMonth.put(i, new HashSet<>());
        }

        for (MessageEntity message : messages) {
            int month = message.getDate().getMonthValue();
            uniqueUsersPerMonth.get(month).add(message.getReceiver().getId());
        }

        uniqueUsersPerMonth.forEach((month, users) -> monthlyCount.put(month, (long) users.size()));

        return monthlyCount.entrySet().stream()
                .map(entry -> Map.<String, Object>of(
                        "year", year,
                        "month", entry.getKey(),
                        "count", entry.getValue()))
                .collect(Collectors.toList());
    }

}
