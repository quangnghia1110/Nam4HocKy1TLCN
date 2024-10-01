package studentConsulting.service.implement.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.payload.dto.statistic.AdvisorStatisticsDTO;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.statistic.StatisticsRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorStatisticsService;

import java.time.LocalDate;
import java.util.List;

@Service
public class AdvisorStatisticsServiceImpl implements IAdvisorStatisticsService {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private DepartmentRepository departmentRepository;
    @Autowired
    private StatisticsRepository statisticsRepository;

    @Override
    public AdvisorStatisticsDTO getAdvisorStatistics(Integer advisorId) {
        AdvisorStatisticsDTO statistics = new AdvisorStatisticsDTO();
        LocalDate today = LocalDate.now();

        List<Integer> departmentIds = departmentRepository.findDepartmentsByManagerId(advisorId);

        statistics.setTotalQuestionsInDay(statisticsRepository.countQuestionsByDepartmentIdsAndDate(departmentIds, today));
        statistics.setTotalForwardedQuestions(statisticsRepository.countDistinctToDepartmentsByManagerAndStatusForwardedTrue(departmentIds));
        statistics.setTotalDeletedQuestions(statisticsRepository.countByDepartmentIdsAndDeletedTrue(departmentIds));
        statistics.setTotalAnswersGiven(statisticsRepository.countByDepartmentIdsAndAnsweredTrue(departmentIds));
        statistics.setTotalAnswerApproval(statisticsRepository.countByDepartmentIdsAndStatusApprovalTrue(departmentIds));
        statistics.setTotalConsultantSchedule(statisticsRepository.countByDepartmentIdsAndStatusConfirmedTrue(departmentIds));
        statistics.setTotalApprovedPosts(statisticsRepository.countByAdvisorIdAndPublishedTrue(advisorId));
        statistics.setTotalConversations(statisticsRepository.countByDepartmentIds(departmentIds));
        statistics.setTotalRatings(statisticsRepository.countRatingsByDepartmentIds(departmentIds));
        statistics.setTotalCommonQuestions(statisticsRepository.countCommonQuestionsByDepartmentIds(departmentIds));
        statistics.setTotalConsultants(statisticsRepository.countConsultantsByDepartmentIds(departmentIds));

        return statistics;
    }

}
