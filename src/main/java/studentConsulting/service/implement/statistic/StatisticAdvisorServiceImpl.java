package studentConsulting.service.implement.statistic;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.CommonQuestionEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.rating.RatingEntity;
import studentConsulting.model.payload.dto.statistic.AdvisorStatisticsDTO;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.CommonQuestionRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.rating.RatingRepository;
import studentConsulting.repository.statistic.StatisticsRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.statistic.IStatisticAdvisorService;
import studentConsulting.specification.communication.ConversationSpecification;
import studentConsulting.specification.consultation_schedule.ConsultationScheduleSpecification;
import studentConsulting.specification.question_answer.AnswerSpecification;
import studentConsulting.specification.question_answer.CommonQuestionSpecification;
import studentConsulting.specification.question_answer.QuestionSpecification;
import studentConsulting.specification.rating.RatingSpecification;

import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class StatisticAdvisorServiceImpl implements IStatisticAdvisorService {

    @Autowired
    private StatisticsRepository statisticsRepository;

    @Autowired
    private QuestionRepository questionRepository;

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

    @Override
    public AdvisorStatisticsDTO getAdvisorStatistics(Integer advisorId, boolean isAdmin) {
        AdvisorStatisticsDTO statistics = new AdvisorStatisticsDTO();
        LocalDate today = LocalDate.now();
        List<Integer> departmentIds = isAdmin ? null : departmentRepository.findDepartmentsByManagerId(advisorId);

        statistics.setTotalQuestionsInDay(statisticsRepository.countQuestionsByDepartmentIdsAndDate(departmentIds, today));
        statistics.setTotalForwardedQuestions(statisticsRepository.countDistinctToDepartmentsByManagerAndStatusForwardedTrue(departmentIds));
        statistics.setTotalDeletedQuestions(statisticsRepository.countByDepartmentIdsAndDeletedTrue(departmentIds));
        statistics.setTotalAnswersGiven(statisticsRepository.countByDepartmentIdsAndAnsweredTrue(departmentIds));
        statistics.setTotalAnswerApproval(statisticsRepository.countByDepartmentIdsAndStatusApprovalTrue(departmentIds));
        statistics.setTotalConsultantSchedule(statisticsRepository.countByDepartmentIdsAndStatusConfirmedTrue(departmentIds));
        statistics.setTotalConversations(statisticsRepository.countByDepartmentIds(departmentIds));
        statistics.setTotalRatings(statisticsRepository.countRatingsByDepartmentIds(departmentIds));
        statistics.setTotalCommonQuestions(statisticsRepository.countCommonQuestionsByDepartmentIds(departmentIds));
        statistics.setTotalConsultants(statisticsRepository.countConsultantsByDepartmentIds(departmentIds));

        return statistics;
    }

    @Override
    public List<Map<String, Object>> getDeletedQuestionsByYear(Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.isDeletedByConsultant())
                .and(QuestionSpecification.hasExactYear(year));

        if (departmentId != null) {
            spec = spec.and(QuestionSpecification.hasDepartmentId(departmentId));
        }

        List<QuestionEntity> questionEntities = questionRepository.findAll(spec);

        for (QuestionEntity question : questionEntities) {
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
    public List<Map<String, Object>> getAnswersGivenByYear(Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<AnswerEntity> spec = Specification.where(AnswerSpecification.hasExactYear(year))
                .and(AnswerSpecification.hasDepartment(departmentId));

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
    public List<Map<String, Object>> getAnswerApprovalByYear(Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<AnswerEntity> spec = Specification.where(AnswerSpecification.hasExactYear(year))
                .and(AnswerSpecification.isApproved())
                .and(AnswerSpecification.hasDepartment(departmentId));

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
    public List<Map<String, Object>> getConsultationSchedulesConsultantByYear(Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasExactYear(year))
                .and(ConsultationScheduleSpecification.hasDepartment(departmentId));

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
    public List<Map<String, Object>> getConversationsConsultantByYear(Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<ConversationEntity> spec = Specification.where(ConversationSpecification.hasExactYear(year))
                .and(ConversationSpecification.hasDepartment(departmentId));

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
    public List<Map<String, Object>> getRatingsByYear(Integer departmentId, Integer year) {
        Map<Integer, Long> monthlyCount = new HashMap<>();
        for (int i = 1; i <= 12; i++) {
            monthlyCount.put(i, 0L);
        }

        Specification<RatingEntity> spec = Specification.where(RatingSpecification.hasExactYear(year))
                .and(RatingSpecification.hasDepartment(departmentId));

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
}
